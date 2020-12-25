#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  where
--------------------------------------------------------------------------------
import Data.List                    (sort)
import Data.Map                     (Map)
import Data.Maybe                   (fromJust)
import Data.Set                     (Set)
import Data.Text                    (Text)
import Data.Void                    (Void)
import Data.Vector                  (Vector, (!))
import Prelude               hiding ((^^))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
--------------------------------------------------------------------------------
import qualified Data.Map     as M
import qualified Data.Set     as S
import qualified Data.Text.IO as T  (readFile)
import qualified Data.Vector  as V
--------------------------------------------------------------------------------
-- the model

type Piece  = Tile Pixel
type Puzzle = [Piece]

data Tile a = Tile { tileId   :: Int
                   , height   :: Int  -- y
                   , width    :: Int  -- x
                   , pixels   :: Vector a
                   }

instance Eq (Tile a) where
  t1 == t2 = tileId t1 == tileId t2

instance Ord (Tile a) where
  t1 <= t2 = tileId t1 <= tileId t2

instance Show a => Show (Tile a) where
  show tile@Tile{..} = "Tile " ++ show tileId ++ ":\n" ++ showPixels pixels
    where
      showPixels pixels | null pixels = ""
                        | otherwise = V.foldr (\p s -> show p ++ s) "\n" (V.take width pixels)
                                   ++ showPixels (V.drop width pixels)

at :: Tile a -> Coordinate -> a
at Tile{..} (Coordinate x y) = pixels ! ( y * width + x )

data Pixel = Black
           | White
  deriving (Eq, Ord)

instance Show Pixel where
  show Black = "#"
  show White = "."

data Coordinate = Coordinate Int Int deriving (Eq, Ord)

instance Show Coordinate where
  show (Coordinate x y) = "(" ++ show x ++ "," ++ show y ++ ")"

inBounds :: Coordinate -> Tile a -> Bool
inBounds (Coordinate x y) Tile{..} = 0 <= x && x < width
                                  && 0 <= y && y < height


--------------------------------------------------------------------------------
-- parsing

type Parser = Parsec Void Text

puzzleP:: Parser Puzzle
puzzleP = sepBy tileP eol <* eof
  where
    tileP :: Parser Piece
    tileP = do
      string "Tile "
      tileId <- decimal <?> "tileId"
      char ':'
      eol
      rows <- some (some pixelP <* eol) <?> "rows"

      let height = length                rows
          width  = length . head       $ rows
          pixels = V.fromList . concat $ rows

      return Tile{..}

    pixelP :: Parser Pixel
    pixelP = choice [ char '#' >> return Black
                    , char '.' >> return White
                    ] <?> "pixelP"

--------------------------------------------------------------------------------
-- features and transformations

top, bottom, left, right :: Tile a -> Vector a
top    Tile{..} = V.take width pixels
bottom Tile{..} = V.drop (width * (height-1)) pixels
left   Tile{..} = V.fromList [ pixels ! (y*width)   | y <- [ 0 .. height-1 ] ]
right  Tile{..} = V.fromList [ pixels ! (y*width-1) | y <- [ 1 .. height   ] ] -- y is running from 1 to height here!

features :: (Ord a) => Tile a -> Set (Vector a)
features t = S.fromList [ top    t, V.reverse (top    t)
                        , bottom t, V.reverse (bottom t)
                        , left   t, V.reverse (left   t)
                        , right  t, V.reverse (right  t)
                        ]

rotate :: Tile a -> Tile a
rotate tile@Tile{..} =
  tile { pixels = V.fromList [ tile `at` Coordinate (width-y-1) x
                             | y <- [ 0 .. height-1 ]
                             , x <- [ 0 .. width-1  ]
                             ]
       }

flipVertically :: Tile a -> Tile a
flipVertically tile@Tile{..} =
  tile { pixels = V.fromList [ tile `at` Coordinate (width-x-1) y
                             | y <- [ 0 .. height-1 ]
                             , x <- [ 0 .. width-1  ]
                             ]
       }

flipHorizontally :: Tile a -> Tile a
flipHorizontally tile@Tile{..} =
  tile { pixels = V.fromList [ tile `at` Coordinate x (height-y-1)
                             | y <- [ 0 .. height-1 ]
                             , x <- [ 0 .. width-1  ]
                             ]
       }

orientations :: Tile a -> [Tile a]
orientations t = [                                               t
                 ,                   rotate                      t
                 ,          rotate . rotate                    $ t
                 , rotate . rotate . rotate                    $ t
                 ,                            flipHorizontally   t
                 ,                   rotate . flipHorizontally $ t
                 ,          rotate . rotate . flipHorizontally $ t
                 , rotate . rotate . rotate . flipHorizontally $ t
                 ,                            flipVertically     t
                 ,                   rotate . flipVertically   $ t
                 ,          rotate . rotate . flipVertically   $ t
                 , rotate . rotate . rotate . flipVertically   $ t
                 ]

crop :: Tile a -> Tile a
crop tile@Tile{..} =
  tile { width  = width  - 2
       , height = height - 2
       , pixels = V.fromList [ tile `at` Coordinate x y
                             | y <- [ 1 .. height-2 ]
                             , x <- [ 1 .. width-2  ]
                             ]
       }

-- append a Tile to the right
(<+) :: Tile a -> Tile a -> Tile a
t1@(Tile tileId1 height1 width1 pixels1) <+ t2@(Tile tileId2 height2 width2 pixels2) =
  if height1 /= height2
    then error $ "mismatching heights in (<+): " ++ show height1 ++ " and " ++ show height2
    else let tileId = tileId1 + tileId2
             height = height1
             width  = width1 + width2
             pixels = V.fromList [ if x < width1
                                     then t1 `at` Coordinate  x           y
                                     else t2 `at` Coordinate (x - width1) y
                                 | y <- [ 0 .. height-1 ]
                                 , x <- [ 0 .. width-1  ]
                                 ]
         in Tile{..}

-- append a Tile to the bottom
(^^) :: Tile a -> Tile a -> Tile a
t1@(Tile tileId1 height1 width1 pixels1) ^^ t2@(Tile tileId2 height2 width2 pixels2) =
  if width1 /= width2
    then error $ "mismatching widths in (^^): " ++ show width1 ++ " and " ++ show width2
    else let tileId = tileId1 + tileId2
             height = height1 + height2
             width  = width1
             pixels = pixels1 <> pixels2
         in Tile{..}

--------------------------------------------------------------------------------

matches :: Piece -> Piece -> Int
matches t1 t2 = S.size $ features t1 `S.intersection` features t2

findMatches :: Puzzle -> [(Piece, [Piece])]
findMatches puzzle = [ (t, [ t' | t' <- puzzle, t' /= t, t' `matches` t > 0 ])
                     | t <- puzzle
                     ]

layPuzzle :: Puzzle -> Piece
layPuzzle puzzle =
      -- find the matching pieces for each piece
  let matching = sort (findMatches puzzle)

      -- declare the first corner we find the top left corner
      (topLeft, [rightOf, below]) = head . filter ((==2) . length . snd) $ matching
      -- and orient it "correctly"
      topLeft' = head [ o
                      | o <- orientations topLeft
                      , S.size (S.singleton (right  o) `S.intersection` features rightOf) == 1
                      , S.size (S.singleton (bottom o) `S.intersection` features below  ) == 1
                      ]
      solution = ((0,0),topLeft') : [((x,y), matchPiece x y solution matching) | y <- [0..11], x <- [0..11], not (x == 0 && y == 0)]
  in glue (map snd solution)
    where
      matchPiece :: Int -> Int -> [((Int,Int), Piece)] -> [(Piece, [Piece])] -> Piece
      matchPiece 0 y solution matching =
        let above = fromJust $ (0,y-1) `lookup` solution
        in head [ t
                | t <- concatMap orientations (fromJust $ above `lookup` matching)
                , bottom above == top t
                ]
      matchPiece x y solution matching =
        let leftOf = fromJust $ (x-1,y) `lookup` solution
        in head [ t
                | t <- concatMap orientations (fromJust $ leftOf `lookup` matching)
                , right leftOf == left t
                ]

      glue :: Puzzle -> Piece
      glue [] = Tile 1 0 (8*12) V.empty
      glue p  = foldr1 (<+) (map crop . take 12 $ p)
             ^^ glue (drop 12 p)

{-
           1111111111
 01234567890123456789
0                  #
1#    ##    ##    ###
2 #  #  #  #  #  #
-}
-- returns the coordinates that belong to a sea monster, they could be overlapping
seaMonsterCoordinates :: Piece -> Coordinate -> [Coordinate]
seaMonsterCoordinates tile (Coordinate x y) =
  let seaMonsterCoordinates = [ Coordinate (x+18)  y                                           ]
                           ++ [ Coordinate (x+x') (y+1) | x' <- [ 0, 5, 6, 11, 12, 17, 18, 19] ]
                           ++ [ Coordinate (x+x') (y+2) | x' <- [ 1, 4, 7, 10, 13, 16        ] ]
  in if all (`inBounds` tile)      seaMonsterCoordinates
     && all ((== Black) . at tile) seaMonsterCoordinates
       then seaMonsterCoordinates
       else []

--------------------------------------------------------------------------------

main :: IO ()
main = do
  input <- parse puzzleP "input.txt" <$> T.readFile "input.txt"

  case input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right puzzle -> do
      let matching = findMatches puzzle
          corners  = filter ((==2) . length . snd) matching

      putStrLn $ "(1) " ++ show (product . map (tileId . fst) $ corners)

      let solution@Tile{..} = layPuzzle puzzle
          -- count the number of sea monster pixels for each orientation of the assembled puzzle
          seaMonsterPixels  = maximum
                            . (flip map) (orientations solution)
                            $ \solution -> S.size . S.fromList . concatMap (seaMonsterCoordinates solution)
                                         $ [ Coordinate x y
                                         | y <- [ 0 .. height - 1 ]
                                         , x <- [ 0 .. width  - 1 ]
                                         ]
          -- "black" pixels in the entire image
          blackPixels       = length . filter (== Black) . V.toList $ pixels
      putStrLn $ "(2) " ++ show (blackPixels - seaMonsterPixels)
