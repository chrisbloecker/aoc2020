#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

{-# LANGUAGE OverloadedStrings #-}

module Main
  where
--------------------------------------------------------------------------------
import Data.Set                     (Set, (\\))
import Data.Text                    (Text)
import Data.Void                    (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
--------------------------------------------------------------------------------
import qualified Data.Text.IO as T (readFile)
import qualified Data.Set     as S
--------------------------------------------------------------------------------

data Tile = Tile Int Int deriving (Show, Eq, Ord)

data Step = E | SE | SW | W | NW | NE deriving (Show, Enum, Bounded)
type Path = [Step]

type Parser = Parsec Void Text

stepP:: Parser Step
stepP = choice [ char   'e'  >> return E
               , char   'w'  >> return W
               , string "se" >> return SE
               , string "sw" >> return SW
               , string "nw" >> return NW
               , string "ne" >> return NE
               ]

--------------------------------------------------------------------------------

step :: Step -> Tile -> Tile
step E  (Tile x y) = Tile (x+1)  y
step SE (Tile x y) = Tile (x+1) (y-1)
step SW (Tile x y) = Tile  x    (y-1)
step W  (Tile x y) = Tile (x-1)  y
step NW (Tile x y) = Tile (x-1) (y+1)
step NE (Tile x y) = Tile  x    (y+1)

neighbours :: Tile -> Set Tile
neighbours t = S.fromList $ map (flip step t) [minBound .. maxBound]

path :: [Step] -> Tile -> Tile
path ss t = foldr step t ss

flipTile :: Tile -> Set Tile -> Set Tile
flipTile t s = if t `S.member` s then t `S.delete` s else t `S.insert` s

day :: Set Tile -> Set Tile
day black = let white = foldr S.union S.empty (S.map neighbours black) \\ black
            in (S.filter remainBlack black) `S.union` (S.filter turnBlack white)
  where
    remainBlack :: Tile -> Bool
    remainBlack t = let s = S.size $ neighbours t `S.intersection` black
                    in s == 1 || s == 2

    turnBlack :: Tile -> Bool
    turnBlack t = let s = S.size $ neighbours t `S.intersection` black
                  in s == 2

--------------------------------------------------------------------------------

main :: IO ()
main = do
  input <- parse (many (many stepP <* eol) <* eof) "input.txt" <$> T.readFile "input.txt"

  case input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right paths -> do
      let black = foldr flipTile S.empty
                . map (flip path (Tile 0 0))
                $ paths
      putStrLn $ "(1) " ++ show (S.size black)

      putStrLn $ "(2) " ++ show (S.size . foldr (.) id (replicate 100 day) $ black)
