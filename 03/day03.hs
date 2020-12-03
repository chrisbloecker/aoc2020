#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

{-# LANGUAGE RecordWildCards #-}

module Main
  where
--------------------------------------------------------------------------------
import Control.Applicative               ((<|>))
import Control.Monad                     (forM_)
import Data.Attoparsec.ByteString        (Parser, parseOnly)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString                   (ByteString)
import Data.Vector                       (Vector, (!), fromList)
--------------------------------------------------------------------------------
import qualified Data.ByteString as BS   (readFile)
--------------------------------------------------------------------------------

data Matrix a = Matrix { height :: Int
                       , width  :: Int
                       , matrix :: Vector a
                       }
  deriving (Show)

at :: Matrix a -> (Int, Int) -> a
at Matrix{..} (x, y) = matrix ! ((y `mod` height) * width + (x `mod` width))

data Field = Free
           | Tree
  deriving (Show, Eq)

--------------------------------------------------------------------------------

parseInput :: ByteString -> Either String (Matrix Field)
parseInput = parseOnly $ do
  rows <- many' $ do cols <- many' (free <|> tree)
                     endOfLine
                     return cols
  endOfInput

  let height = length rows
      width  = length . head $ rows
      matrix = fromList . concat $ rows

  return Matrix{..}

    where
      free :: Parser Field
      free = char '.' >> return Free

      tree :: Parser Field
      tree = char '#' >> return Tree

--------------------------------------------------------------------------------

slope1, slope2, slope3, slope4, slope5 :: Int -> [(Int, Int)]
slope1 h = [(y,        y) | y <- [0  ..(h-1)]]
slope2 h = [(3*y,      y) | y <- [0  ..(h-1)]]
slope3 h = [(5*y,      y) | y <- [0  ..(h-1)]]
slope4 h = [(7*y,      y) | y <- [0  ..(h-1)]]
slope5 h = [(y `div` 2,y) | y <- [0,2..(h-1)]]

main :: IO ()
main = do
  input <- parseInput <$> BS.readFile "input.txt"

  case input of
    Left err -> putStrLn err
    Right m@Matrix{..} ->
      forM_ ([1..] `zip` [slope1, slope2, slope3, slope4, slope5]) $ \(x, slope) ->
        let count = length . filter (==Tree) . map (at m) $ slope height
        in putStrLn . concat $ ["slope ", show x, ": ", show count]
