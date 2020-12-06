#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

module Main
  where
--------------------------------------------------------------------------------
import Data.Text                         (Text)
import Data.Void                         (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
--------------------------------------------------------------------------------
import qualified Data.Text.IO as T       (readFile)
import qualified Data.Set     as S       (fromList, size, intersection)
--------------------------------------------------------------------------------

type Answer       = Char
type Answers      = [Answer]
type GroupAnswers = [Answers]

type Parser = Parsec Void Text

parseInput :: Parser [GroupAnswers]
parseInput = sepBy (many (some letterChar <* eol)) eol

--------------------------------------------------------------------------------

main :: IO ()
main = do
  input <- parse parseInput "input.txt" <$> T.readFile "input.txt"

  case input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right groupAnswers -> do
      putStr "(1) "
      print . sum . map (S.size . S.fromList . concat) $ groupAnswers

      putStr "(2) "
      print . sum . map (S.size . foldr (S.intersection . S.fromList) (S.fromList ['a'..'z'])) $ groupAnswers
