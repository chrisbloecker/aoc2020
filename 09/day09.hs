#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

module Main
  where
--------------------------------------------------------------------------------
import Data.List                         (elem)
import Data.Text                         (Text)
import Data.Void                         (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
--------------------------------------------------------------------------------
import qualified Data.Text.IO as T       (readFile)
import qualified Data.Set     as S       (fromList, size, intersection)
--------------------------------------------------------------------------------

type Parser = Parsec Void Text

parseInput :: Parser [Int]
parseInput = many (decimal <* eol) <* eof

--------------------------------------------------------------------------------

findInvalid :: [Int] -> [Int]
findInvalid xs = go (take 25 xs) (drop 25 xs)
  where
    go _  []     = []
    go ns (y:ys) = if not (isComposite y ns)
                     then y : go (tail ns ++ [y]) ys
                     else     go (tail ns ++ [y]) ys

    isComposite :: Int -> [Int] -> Bool
    isComposite x xs = x `elem` [y+z | y <- xs, z <- xs, y /= z]

findContiguous :: [Int] -> Int -> [Int]
findContiguous xs s = case scan xs s 0 0 of
  Nothing -> findContiguous (tail xs) s
  Just n  -> take n xs
  where
    scan :: [Int] -> Int -> Int -> Int -> Maybe Int
    scan (x:xs) s a r
      | a <  s = scan xs s (a+x) (r+1)
      | a == s = Just r
      | a >  s = Nothing

main :: IO ()
main = do
  input <- parse parseInput "input.txt" <$> T.readFile "input.txt"

  case input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right numbers -> do
      let theNumber = head . findInvalid $ numbers
      print theNumber

      let theNumbers = findContiguous numbers theNumber
      print (minimum theNumbers + maximum theNumbers)
