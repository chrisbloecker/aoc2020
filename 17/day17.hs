#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

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
import qualified Data.Vector  as V  (take, drop, foldr, null, filter, length, fromList, toList)
--------------------------------------------------------------------------------

data Grid a = Grid { height :: Int  -- y
                   , width  :: Int  -- x
                   , depth  :: Int  -- z
                   --, time   :: Int  -- w
                   , grid   :: Vector a
                   }
  deriving (Eq)

-- ignoring time-coordinate
instance Show a => Show (Grid a) where
  show g@Grid{..} | null grid = ""
                  | otherwise = let layer = V.take (width * height) grid
                                in showLayer layer
                                ++ show g { grid = V.drop (width * height) grid }
    where
      showLayer layer | null layer = "\n"
                      | otherwise  = V.foldr (\e s -> show e ++ s) "\n" (V.take width layer)
                                  ++ showLayer (V.drop width layer)

at :: Grid a -> Coordinate -> a
at Grid{..} (Coordinate x y z) = grid ! ( z * width * height
                                        + y * width
                                        + x
                                        )

data Cube = Active
          | Inactive
  deriving (Eq)

instance Show Cube where
  show Active   = "#"
  show Inactive = "."

data Coordinate = Coordinate Int Int Int

instance Show Coordinate where
  show (Coordinate x y z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

--------------------------------------------------------------------------------

type Parser = Parsec Void Text

parseInput :: Parser (Grid Cube)
parseInput = do
  rows <- sepBy (many cube) eol <* eof

  let height = length . filter (not . null) $ rows
      width  = length . head $ rows
      depth  = 1
      grid   = V.fromList . concat $ rows

  return Grid{..}

    where
      cube :: Parser Cube
      cube = choice [ char '#' >> return Active
                    , char '.' >> return Inactive
                    ]

--------------------------------------------------------------------------------

inBounds :: Coordinate -> Grid a -> Bool
inBounds (Coordinate x y z) Grid{..} = 0 <= x && x < width
                                    && 0 <= y && y < height
                                    && 0 <= z && z < depth

step :: Grid Cube -> Grid Cube
step g@Grid{..} = Grid { width  = width  + 2
                       , height = height + 2
                       , depth  = depth  + 2
                       , grid   = V.fromList [ let n = c `activeNeighbours` g
                                               in if c `inBounds` g
                                                    then case g `at` c of
                                                      Inactive -> if           n == 3 then Active else Inactive
                                                      Active   -> if n == 2 || n == 3 then Active else Inactive
                                                    else if n == 3 then Active else Inactive
                                             | z <- [ -1 .. depth  ]
                                             , y <- [ -1 .. height ]
                                             , x <- [ -1 .. width  ]
                                             , c <- [ Coordinate x y z ]
                                             ]
                       }
  where
    activeNeighbours :: Coordinate -> Grid Cube -> Int
    activeNeighbours c@(Coordinate x y z) g = length
                                            . filter (== Active)
                                            $ [ if c' `inBounds` g
                                                  then g `at` c'
                                                  else Inactive
                                              | z' <- [ z-1, z, z+1 ]
                                              , y' <- [ y-1, y, y+1 ]
                                              , x' <- [ x-1, x, x+1 ]
                                              , not (z' == z && y' == y && x' == x)
                                              , c' <- [ Coordinate x' y' z' ]
                                              ]

main :: IO ()
main = do
  input <- parse parseInput "input.txt" <$> T.readFile "small.txt"

  case input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right g -> do
      let g3 = foldr (.) id (replicate 6 step) g
      putStrLn $ "(1) " ++ show (length . filter (== Active) . V.toList . grid $ g3)
