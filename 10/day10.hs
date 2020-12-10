#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

module Main
  where
--------------------------------------------------------------------------------
import Data.List                    (sort)
import Data.Text                    (Text)
import Data.Void                    (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
--------------------------------------------------------------------------------
import qualified Data.Text.IO as T  (readFile)
--------------------------------------------------------------------------------

type Parser = Parsec Void Text

parseInput :: Parser [Int]
parseInput = many (decimal <* eol) <* eof

--------------------------------------------------------------------------------

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn y xs = (takeWhile (/=y) . dropWhile (==y)) xs
             : splitOn y (dropWhile (==y) . dropWhile (/=y) . dropWhile (==y) $ xs)

combinations :: (Eq a, Num a) => a -> a
combinations 0 = 0
combinations 1 = 1
combinations 2 = 2
combinations 3 = 4
combinations 4 = 7

main :: IO ()
main = do
  input <- parse parseInput "input.txt" <$> T.readFile "input.txt"

  case input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right numbers -> do
      let joltages = sort (maximum numbers + 3 : numbers)
          gaps     = map ((\x -> -x) . uncurry (-))
                   $ (0 : joltages) `zip` joltages
          sections = splitOn 3 gaps

      putStrLn . ("(1) " ++)
               . show
               $ (length . filter (==1) $ gaps) * (length . filter (==3) $ gaps)

      putStrLn . ("(2) " ++)
               . show
               . product
               . map (combinations . length)
               $ sections
