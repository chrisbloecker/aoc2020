#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

{-# LANGUAGE RecordWildCards #-}

module Main
  where
--------------------------------------------------------------------------------
import Data.Attoparsec.ByteString        ((<?>), Parser, parseOnly)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString                   (ByteString)
import Data.ByteString.Char8             (unpack)
import Data.List                         (elem)
import Prelude                    hiding (takeWhile)
--------------------------------------------------------------------------------
import qualified Data.ByteString as BS   (readFile)
--------------------------------------------------------------------------------

data Pattern = Pattern { pmin  :: Int
                       , pmax  :: Int
                       , pchar :: Char
                       }
  deriving (Show)

parseInput :: ByteString -> Either String [(Pattern, String)]
parseInput = parseOnly $ do
  xs <- many' $ do pmin <- decimal
                   char '-'
                   pmax <- decimal
                   skipSpace
                   pchar <- anyChar
                   char ':'
                   skipSpace
                   pw <- unpack <$> takeWhile (/= '\n')
                   endOfLine
                   return (Pattern{..}, pw)
  endOfInput
  return xs

--------------------------------------------------------------------------------

matches :: Pattern -> String -> Bool
matches Pattern{..} s =
  let l = length . filter (== pchar) $ s
  in pmin <= l && l <= pmax

matches2 :: Pattern -> String -> Bool
matches2 Pattern{..} s = (s !! (pmin-1) == pchar) /= (s !! (pmax-1) == pchar)

main :: IO ()
main = do
  input <- parseInput <$> BS.readFile "input.txt"

  case input of
    Left err -> putStrLn err
    Right xs -> do
      print . length . filter id . map (uncurry matches)  $ xs
      print . length . filter id . map (uncurry matches2) $ xs
