#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

module Main
  where
--------------------------------------------------------------------------------
import Data.Text                    (Text)
import Data.Void                    (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
--------------------------------------------------------------------------------
import qualified Data.Text.IO as T  (readFile)
--------------------------------------------------------------------------------

data Point = Point Int Int deriving (Eq)

instance Show Point where
  show (Point x y) = "Point (" ++ show x ++ "," ++ show y ++ ")"

type Parser = Parsec Void Text

data Instruction = N Int
                 | S Int
                 | E Int
                 | W Int
                 | L Int
                 | R Int
                 | F Int
  deriving (Show)

parseInput :: Parser [Instruction]
parseInput = many (choice [ char 'N' >> N <$> decimal
                          , char 'S' >> S <$> decimal
                          , char 'E' >> E <$> decimal
                          , char 'W' >> W <$> decimal
                          , char 'L' >> L <$> decimal
                          , char 'R' >> R <$> decimal
                          , char 'F' >> F <$> decimal
                          ] <* eol) <* eof

--------------------------------------------------------------------------------

sin' :: Int -> Int
sin' d = round . sin $ pi * fromIntegral d / 180

cos' :: Int -> Int
cos' d = round . cos $ pi * fromIntegral d / 180

rotate :: Int -> Point -> Point
rotate t (Point x y) = Point (x * cos' t - y * sin' t) (x * sin' t + y * cos' t)

translate :: Point -> Point -> Point
translate (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

scale :: Int -> Point -> Point
scale n (Point x y) = Point (n*x) (n*y)

l0 :: Point -> Int
l0 (Point x y) = abs x + abs y

follow :: [Instruction] -> Point
follow instructions = go 0 instructions (Point 0 0)
  where
    go _ []       p = p
    go t (N n:is) p = go t  is $ translate (Point 0 n) p
    go t (S s:is) p = go t  is $ translate (Point 0 (-s)) p
    go t (E e:is) p = go t  is $ translate (Point e 0) p
    go t (W w:is) p = go t  is $ translate (Point (-w) 0) p
    go t (L l:is) p = go (t + l) is p
    go t (R r:is) p = go (t - r) is p
    go t (F f:is) p = go t  is $ translate (rotate t (Point f 0)) p

waypoint :: [Instruction] -> Point
waypoint instructions = go instructions (Point 0 0) (Point 10 1)
  where
    go []       p wp = p
    go (N n:is) p wp = go is p (translate (Point 0 n) wp)
    go (S s:is) p wp = go is p (translate (Point 0 (-s)) wp)
    go (E e:is) p wp = go is p (translate (Point e 0) wp)
    go (W w:is) p wp = go is p (translate (Point (-w) 0) wp)
    go (L l:is) p wp = go is p (rotate l wp)
    go (R r:is) p wp = go is p (rotate (-r) wp)
    go (F f:is) p wp = go is (translate p (scale f wp)) wp

main :: IO ()
main = do
  input <- parse parseInput "input.txt" <$> T.readFile "input.txt"

  case input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right instructions -> do
      print . l0 . follow $ instructions
      print . l0 . waypoint $ instructions
