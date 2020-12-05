#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

{-# LANGUAGE RecordWildCards #-}

module Main
  where
--------------------------------------------------------------------------------
import Control.Monad                     (forM, replicateM)
import Data.List                         (elem, notElem)
import Data.Text                         (Text)
import Data.Void                         (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
--------------------------------------------------------------------------------
import qualified Data.Text.IO as T       (readFile)
--------------------------------------------------------------------------------

data SeatAddr = SeatAddr { row :: [Int]
                         , col :: [Int]
                         }
  deriving (Show)

seatToDecimal :: SeatAddr -> (Int, Int)
seatToDecimal SeatAddr{..} = ( convert (reverse row)
                             , convert (reverse col)
                             )
  where
    convert :: [Int] -> Int
    convert bs = foldr (\(ix,b) s -> s+b*2^ix) 0 ([0..] `zip` bs)

seatID :: SeatAddr -> Int
seatID seatAddr = let (rowID, colID) = seatToDecimal seatAddr
                  in 8*rowID + colID

--------------------------------------------------------------------------------

type Parser = Parsec Void Text

parseInput :: Parser [SeatAddr]
parseInput = many $ do row <- replicateM 7 rowAddr
                       col <- replicateM 3 colAddr
                       eol
                       return SeatAddr {..}

  where
    rowAddr :: Parser Int
    rowAddr = choice [ char 'F' >> return 0
                     , char 'B' >> return 1
                     ]

    colAddr :: Parser Int
    colAddr = choice [ char 'L' >> return 0
                     , char 'R' >> return 1
                     ]

--------------------------------------------------------------------------------

main :: IO ()
main = do
  input <- parse parseInput "input.txt" <$> T.readFile "input.txt"

  case input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right seats -> do
      let seatIDs = map seatID seats

      putStr "(1) Maximum seat ID: "
      print (maximum seatIDs)

      putStr "(2) My seat ID:      "
      print . head $  [ seatID | seatID <- [1..maximum seatIDs]
                               , seatID   `notElem` seatIDs
                               , seatID-1 `elem`    seatIDs
                               , seatID+1 `elem`    seatIDs
                      ]
