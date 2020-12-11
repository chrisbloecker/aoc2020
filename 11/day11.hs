#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  where
--------------------------------------------------------------------------------
import Data.Text                    (Text)
import Data.Void                    (Void)
import Data.Vector                  (Vector, (!))
import Text.Megaparsec
import Text.Megaparsec.Char
--------------------------------------------------------------------------------
import qualified Data.Text.IO as T  (readFile)
import qualified Data.Vector  as V  (take, drop, foldr, null, filter, length, fromList)
--------------------------------------------------------------------------------

data Matrix a = Matrix { height :: Int
                       , width  :: Int
                       , matrix :: Vector a
                       }
  deriving (Eq)

instance Show a => Show (Matrix a) where
  show m@Matrix{..} =
    if null matrix
      then ""
      else V.foldr (\e s -> show e ++ s) "\n" (V.take width matrix) ++
           show m { matrix = V.drop width matrix }

at :: Matrix a -> Seat -> a
at Matrix{..} (Seat x y) = matrix ! ((y `mod` height) * width + (x `mod` width))

data Cell = Free
          | Taken
          | Floor
  deriving (Eq)

instance Show Cell where
  show Free  = "L"
  show Taken = "#"
  show Floor = "."

data Seat = Seat Int Int

data Direction = N | NE | E | SE | S | SW | W | NW deriving (Enum, Bounded)

(+>) :: Seat -> Direction -> Seat
(+>) (Seat x y) = \case N  -> Seat  x    (y-1)
                        NE -> Seat (x+1) (y-1)
                        E  -> Seat (x+1)  y
                        SE -> Seat (x+1) (y+1)
                        S  -> Seat  x    (y+1)
                        SW -> Seat (x-1) (y+1)
                        W  -> Seat (x-1)  y
                        NW -> Seat (x-1) (y-1)

inBounds :: Matrix a -> Seat -> Bool
inBounds Matrix{..} (Seat x y) = 0 <= x && x < width
                              && 0 <= y && y < height

type DecisionRule = Int -> Bool
type Neighbours   = Matrix Cell -> Seat -> [Seat]

--------------------------------------------------------------------------------

type Parser = Parsec Void Text

parseInput :: Parser (Matrix Cell)
parseInput = do
  rows <- sepBy (many cell) eol <* eof

  let height = length . filter (not . null) $ rows
      width  = length . head $ rows
      matrix = V.fromList . concat $ rows

  return Matrix{..}

    where
      cell :: Parser Cell
      cell = choice [ char 'L' >> return Free
                    , char '#' >> return Taken
                    , char '.' >> return Floor
                    ]

--------------------------------------------------------------------------------

directNeighbours :: Neighbours
directNeighbours m s = [ s +> d | d <- [minBound .. maxBound], inBounds m (s +> d)]

inLineOfSight :: Neighbours
inLineOfSight m s = [ s >+ d | d <- [minBound .. maxBound], inBounds m (s >+ d)]
  where
    (>+) :: Seat -> Direction -> Seat
    s >+ d = let s' = s +> d
             in if not (inBounds m s') || at m s' /= Floor
                  then s'
                  else s' >+ d

fixpoint :: Neighbours -> DecisionRule -> DecisionRule -> Matrix Cell -> Matrix Cell
fixpoint neighbours takeIf leaveIf m@Matrix{..} = go m
  where
    go :: Matrix Cell -> Matrix Cell
    go m = let next = m { matrix = V.fromList [takeOrLeave m seat | seat <- seats] }
           in if m == next
                then m
                else go next

    takeOrLeave :: Matrix Cell -> Seat -> Cell
    takeOrLeave m s = let takenNeighbours = length [ () | n <- neighbours m s , m `at` n == Taken]
                      in case m `at` s of
                           Free  -> if takeIf  takenNeighbours then Taken else Free
                           Taken -> if leaveIf takenNeighbours then Free  else Taken
                           Floor -> Floor

    seats :: [Seat]
    seats = [Seat x y | y <- [0 .. height-1], x <- [0 .. width-1]]

main :: IO ()
main = do
  input <- parse parseInput "input.txt" <$> T.readFile "input.txt"

  case input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right m -> do
      let fp1 = fixpoint directNeighbours (== 0) (>= 4) m
      putStrLn $ "(1) " ++ show (V.length . V.filter (== Taken) . matrix $ fp1)

      let fp2 = fixpoint inLineOfSight    (== 0) (>= 5) m
      putStrLn $ "(2) " ++ show (V.length . V.filter (== Taken) . matrix $ fp2)
