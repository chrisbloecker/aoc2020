#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  where
--------------------------------------------------------------------------------
import Data.Set                     (Set)
import Data.Text                    (Text)
import Data.Void                    (Void)
import Data.Vector                  (Vector, (!))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
--------------------------------------------------------------------------------
import qualified Data.Set     as S  (fromList, intersection, size)
import qualified Data.Text.IO as T  (readFile)
import qualified Data.Vector  as V  (take, drop, foldr, null, filter, length, fromList, toList, reverse)
--------------------------------------------------------------------------------

type Puzzle = [Tile Pixel]

data Tile a = Tile { tileId   :: Int
                   , height   :: Int  -- y
                   , width    :: Int  -- x
                   , pixels   :: Vector a
                   , features :: Set (Vector a)
                   }

instance Eq (Tile a) where
  t1 == t2 = tileId t1 == tileId t2

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

data Coordinate = Coordinate Int Int

instance Show Coordinate where
  show (Coordinate x y) = "(" ++ show x ++ "," ++ show y ++ ")"

--------------------------------------------------------------------------------

type Parser = Parsec Void Text

puzzleP:: Parser Puzzle
puzzleP = sepBy tileP eol <* eof
  where
    tileP :: Parser (Tile Pixel)
    tileP = do
      string "Tile "
      tileId <- decimal <?> "tileId"
      char ':'
      eol
      rows <- some (some pixelP <* eol) <?> "rows"

      let height = length                rows
          width  = length . head       $ rows
          pixels = V.fromList . concat $ rows

          firstRow = V.take width pixels
          lastRow  = V.drop (width * (height-1)) pixels
          firstCol = V.fromList [ pixels ! ( y * width )
                                | y <- [ 0 .. height-1 ]
                                ]
          lastCol  = V.fromList [ pixels ! ( y * width + (width - 1) )
                                | y <- [ 0 .. height-1 ]
                                ]

          -- the edges of the tile
          features = S.fromList [ firstRow, V.reverse firstRow
                                , lastRow,  V.reverse lastRow
                                , firstCol, V.reverse firstCol
                                , lastCol,  V.reverse lastCol
                                ]

      return Tile{..}

    pixelP :: Parser Pixel
    pixelP = choice [ char '#' >> return Black
                    , char '.' >> return White
                    ] <?> "pixelP"

--------------------------------------------------------------------------------

matches :: Tile Pixel -> Tile Pixel -> Int
matches t1 t2 = S.size $ features t1 `S.intersection` features t2

main :: IO ()
main = do
  input <- parse puzzleP "input.txt" <$> T.readFile "input.txt"

  case input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right puzzle -> do
      let corners = [ t | t <- puzzle
                    , length [ ()
                             | t' <- puzzle
                             , t' /= t
                             , t `matches` t' > 0
                             ] == 2
                    ]

      putStrLn $ "(1) " ++ show (product . map tileId $ corners)
