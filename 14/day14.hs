#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  where
--------------------------------------------------------------------------------
import Data.List                    (intercalate)
import Data.Map                     (Map)
import Data.Text                    (Text)
import Data.Void                    (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
--------------------------------------------------------------------------------
import qualified Data.Map     as M  (empty, insert)
import qualified Data.Text.IO as T  (readFile)
--------------------------------------------------------------------------------

data Instruction = Mask [Maybe Bool]
                 | Mem  Int Int

instance Show Instruction where
  show (Mask mbs)     = "mask = " ++ intercalate "" (map showB mbs)
    where
      showB Nothing      = "X"
      showB (Just False) = "0"
      showB (Just True)  = "1"
  show (Mem addr val) = "mem[" ++ show addr ++ "] = " ++ show val

data Machine = Machine { mask :: [Maybe Bool]
                       , mem  :: Map Int Int
                       }

instance Show Machine where
  show Machine{..} = "Machine { " ++ show (Mask mask) ++ "\n" ++
                     "        , mem = " ++ show mem   ++ "\n" ++
                     "        }"

--------------------------------------------------------------------------------

type Parser = Parsec Void Text

parseInput :: Parser [Instruction]
parseInput = many (choice [maskIns, memIns]) <* eof
  where
    maskIns = do
      string "mask = "
      mask <- many $ choice [ char 'X' >> return Nothing
                            , char '0' >> return (Just False)
                            , char '1' >> return (Just True)
                            ]
      eol
      return $ Mask mask

    memIns = do
      string "mem["
      addr <- decimal
      string "] = "
      val <- decimal
      eol
      return $ Mem addr val

--------------------------------------------------------------------------------

decToBits :: Int -> [Bool]
decToBits n = [ (n `mod` 2^i) `div` 2^(i-1) > 0 | i <- [36,35..1] ]

bitsToDec :: [Bool] -> Int
bitsToDec bs = sum [ 2^i | (i,b) <- [35,34..] `zip` bs, b ]

initMachine :: Machine
initMachine = Machine (replicate 36 Nothing) M.empty

(->-) :: Machine -> Instruction -> Machine
(->-) machine@Machine{..} = \case
  Mask m  -> machine { mask = m }
  Mem a v -> machine { mem = M.insert a (mask +> v) mem }
    where
      (+>) :: [Maybe Bool] -> Int -> Int
      mbs +> n = bitsToDec (zipWith (><) mbs (decToBits n))

      (><) :: Maybe Bool -> Bool -> Bool
      Nothing >< b = b
      Just b  >< _ = b

(=>=) :: Machine -> Instruction -> Machine
(=>=) machine@Machine{..} = \case
  Mask m  -> machine { mask = m }
  Mem a v -> machine { mem = foldr (\a mem -> M.insert a v mem) mem (mask +> a) }
    where
      (+>) :: [Maybe Bool] -> Int -> [Int]
      mbs +> a = map bitsToDec (mbs >< decToBits a)

      (><) :: [Maybe Bool] -> [Bool] -> [[Bool]]
      [] >< []                   = [[]]
      (Nothing   :mbs) >< (b:bs) = let rest = mbs >< bs
                                   in map (True:) rest ++ map (False:) rest
      (Just False:mbs) >< (b:bs) = map (b:)    (mbs >< bs)
      (Just True :mbs) >< (_:bs) = map (True:) (mbs >< bs)

main :: IO ()
main = do
  input <- parse parseInput "input.txt" <$> T.readFile "input.txt"

  case input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right program -> do
      let machine1 = foldl (->-) initMachine program
      putStrLn $ "(1) " ++ show (sum . mem $ machine1)

      let machine2 = foldl (=>=) initMachine program
      putStrLn $ "(2) " ++ show (sum . mem $ machine2)
