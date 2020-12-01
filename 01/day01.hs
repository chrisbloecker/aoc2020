#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

module Main
  where
--------------------------------------------------------------------------------
import Data.Attoparsec.ByteString        ((<?>), Parser, parseOnly)
import Data.Attoparsec.ByteString.Char8  (decimal, endOfLine, endOfInput, many')
import Data.ByteString                   (ByteString)
import Data.List                         (elem)
--------------------------------------------------------------------------------
import qualified Data.ByteString as BS   (readFile)
--------------------------------------------------------------------------------

parseInput :: ByteString -> Either String [Int]
parseInput text = parseOnly parser text
  where
    parser :: Parser [Int]
    parser = do
      xs <- many' $ decimal <* endOfLine
      endOfInput
      return xs

--------------------------------------------------------------------------------

findTwo :: Int -> [Int] -> Maybe (Int, Int)
findTwo _ []     = Nothing
findTwo s (x:xs) = if s-x `elem` xs
                     then Just (x, s-x)
                     else findTwo s xs

findThree :: Int -> [Int] -> Maybe (Int, Int, Int)
findThree _ []     = Nothing
findThree s (x:xs) = case findTwo (s-x) xs of
  Nothing    -> findThree s xs
  Just (y,z) -> Just (x,y,z)

main :: IO ()
main = do
  input <- parseInput <$> BS.readFile "input.txt"

  case input of
    Left err -> putStrLn err
    Right xs -> do
      print . maybe 0 (uncurry (*))       . findTwo   2020 $ xs
      print . maybe 0 (\(x,y,z) -> x*y*z) . findThree 2020 $ xs
