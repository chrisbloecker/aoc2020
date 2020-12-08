#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

{-# LANGUAGE OverloadedStrings #-}

module Main
  where
--------------------------------------------------------------------------------
import Data.List                    (elem)
import Data.Map                     (Map)
import Data.Maybe                   (catMaybes, fromJust)
import Data.Text                    (Text, pack)
import Data.Void                    (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
--------------------------------------------------------------------------------
import qualified Data.Text.IO as T  (readFile)
import qualified Data.Map     as M  (fromList, lookup, elems)
--------------------------------------------------------------------------------

type Rules = Map Text Bag

data Bag = Bag Text [(Text, Int)]
  deriving (Show)

instance Eq Bag where
  Bag n1 _ == Bag n2 _ = n1 == n2

type Parser = Parsec Void Text

parseInput :: Parser Rules
parseInput = do bags <- many bag
                eof
                return $ M.fromList [(name, bag) | bag@(Bag name _) <- bags]
  where
    bag :: Parser Bag
    bag = do
      name <- bagName
      string "bags contain"
      rules <- rule `sepBy` char ','
      char '.'
      eol
      return $ case rules of
        [("",_)] -> Bag name []
        rs       -> Bag name rules

    bagName :: Parser Text
    bagName = do
      part1 <- takeWhileP (Just "1st part of bag name") (/= ' ')
      char ' '
      part2 <- takeWhileP (Just "2nd part of bag name") (/= ' ')
      char ' '
      return (part1 <> " " <> part2)

    rule :: Parser (Text, Int)
    rule = choice [ string " no other bags" >> return ("", 0)
                  , do char ' '
                       num <- decimal
                       char ' '
                       name <- bagName
                       if num > 1
                          then string "bags"
                          else string "bag"
                       return (name, num)
                  ]

--------------------------------------------------------------------------------

canContain :: Rules -> Bag -> Bag -> Bool
canContain rules theBag@(Bag name _) (Bag _ allowed) =
  name `elem` map fst allowed
    || any (canContain rules theBag)
           [ fromJust $ other `M.lookup` rules
           | (other, _) <- allowed
           ]

countItems :: Rules -> Bag -> Int
countItems _     (Bag _ []    ) = 1
countItems rules (Bag _ others) =
  1 + sum [ amount * countItems rules (fromJust $ name `M.lookup` rules)
          | (name, amount) <- others
          ]

main :: IO ()
main = do
  input <- parse parseInput "input.txt" <$> T.readFile "input.txt"

  case input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right rules -> do
      putStr "(1) How many bags can contain a shiny gold bag? "
      print . length . filter id $ [canContain rules (Bag "shiny gold" []) bag | bag <- M.elems rules]

      putStr "(1) How many items are in a shiny gold bag? "
      print $ (countItems rules . fromJust $ "shiny gold" `M.lookup` rules) - 1
