#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  where
--------------------------------------------------------------------------------
import Data.List                    (transpose)
import Data.Map                     (Map)
import Data.Text                    (Text, unpack)
import Data.Void                    (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
--------------------------------------------------------------------------------
import qualified Data.Text.IO as T  (readFile)
--------------------------------------------------------------------------------

type Range = (Int, Int)

data Rule = Rule { ruleName :: Text
                 , r1       :: Range
                 , r2       :: Range
                 }

instance Show Rule where
  show (Rule ruleName (r1min, r1max) (r2min, r2max)) =
    show ruleName ++ ": " ++ show r1min ++ "-" ++ show r1max ++ " or " ++ show r2min ++ "-" ++ show r2max

type Ticket = [Int]

satisfies :: Int -> Rule -> Bool
satisfies x Rule{..} = x `satisfiesRange` r1
                    || x `satisfiesRange` r2
  where
    satisfiesRange x (min, max) = min <= x && x <= max

--------------------------------------------------------------------------------

type Parser = Parsec Void Text

parseInput :: Parser ([Rule], Ticket, [Ticket])
parseInput = do
  rules <- many (try rule)
  eol
  string "your ticket:"
  eol
  yourTicket <- ticket
  eol
  string "nearby tickets:"
  eol
  nearbyTickets <- some ticket
  eof
  return (rules, yourTicket, nearbyTickets)

    where
      rule :: Parser Rule
      rule = do
        ruleName <- takeWhile1P (Just "ruleName") (/= ':')
        string ": "
        r1 <- range
        string " or "
        r2 <- range
        eol
        return Rule{..}

      range :: Parser Range
      range = do
        rmin <- decimal
        char '-'
        rmax <- decimal
        return (rmin, rmax)

      ticket :: Parser Ticket
      ticket = sepBy decimal (char ',') <* eol

--------------------------------------------------------------------------------

findColumns :: [(Int, [Text])] -> [(Int, Text)]
findColumns [] = []
findColumns xs = let theOne@(ix, name) = findTheOne xs
                 in theOne : findColumns [(ix', filter (/= name) names) | (ix', names) <- xs, ix' /= ix]
  where
    findTheOne :: [(Int, [Text])] -> (Int, Text)
    findTheOne xs = case filter ((== 1) . length . snd) xs of
                      [(ix, [name])] -> (ix, name)
                      _              -> error "cannot find a column that only matches one rule."

main :: IO ()
main = do
  input <- parse parseInput "input.txt" <$> T.readFile "input.txt"

  case input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right (rules, yourTicket, nearbyTickets) -> do
      let violatesAll x = all (not . satisfies x) rules
      let matchesSome x = any (      satisfies x) rules

      putStrLn $ "(1) " ++ show (sum . filter violatesAll . concat $ nearbyTickets)

      let validNearbyTickets = filter (all matchesSome) nearbyTickets
          columns            = transpose validNearbyTickets
          matchedRules       = [ (ix, [ ruleName
                                      | rule@Rule{..} <- rules
                                      , all (`satisfies` rule) column
                                      ]
                                 )
                               | (ix, column) <- [0..] `zip` columns
                               ]
          departureColumns   = [ ix
                               | (ix, columnName) <- findColumns matchedRules
                               , and (zipWith (==) "departure" (unpack columnName))
                               ]

      putStrLn $ "(2) " ++ show (product [yourTicket !! ix | ix <- departureColumns])
