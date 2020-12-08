#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  where
--------------------------------------------------------------------------------
import Control.Monad                (forM_)
import Data.List                    (elem)
import Data.Map                     (Map)
import Data.Maybe                   (catMaybes, fromJust)
import Data.Text                    (Text)
import Data.Void                    (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
--------------------------------------------------------------------------------
import qualified Data.Text.IO as T  (readFile)
import qualified Data.Map     as M  (fromList, lookup, elems)
--------------------------------------------------------------------------------

data Instruction = NOP Int
                 | ACC Int
                 | JMP Int
  deriving (Show)

data ReturnCode = InfiniteLoop Int
                | Terminated   Int
  deriving (Show)

type Program = [Instruction]
type PC      = Int

data Machine = Machine { state :: Int
                       , l     :: [(Instruction, PC)]
                       , r     :: [(Instruction, PC)]
                       }

instance Show Machine where
  show Machine{..} = "Machine { state = " ++ show state ++ "\n" ++
                     "        , l     = " ++ show l     ++ "\n" ++
                     "        , r     = " ++ show r     ++ "\n" ++
                     "        }"

type Parser = Parsec Void Text

parseInput :: Parser Program
parseInput = do instructions <- many $ choice [nop, acc, jmp] <* eol
                eof
                return instructions
  where
    nop :: Parser Instruction
    nop = do
      string "nop "
      unary <- unaryP
      NOP . unary <$> decimal

    acc :: Parser Instruction
    acc = do
      string "acc "
      unary <- unaryP
      ACC . unary <$> decimal

    jmp :: Parser Instruction
    jmp = do
      string "jmp "
      unary <- unaryP
      JMP . unary <$> decimal

    unaryP :: Parser (Int -> Int)
    unaryP = choice [ char '+' >> return id
                    , char '-' >> return (\x -> -x)
                    ]

--------------------------------------------------------------------------------

initMachine :: Program -> Machine
initMachine p = Machine { state = 0
                        , l     = []
                        , r     = p `zip` repeat 0
                        }

step :: Machine -> Machine
step m@Machine{..} = case r of
  []                  -> error "Out of bounds."
  (ins@(NOP _),pc):_ -> m { l = (ins,pc+1) : l
                          , r = tail r
                          }
  (ins@(ACC n),pc):_ -> m { state = state + n
                          , l     = (ins,pc+1) : l
                          , r     = tail r
                          }
  (ins@(JMP n),pc):is -> if n > 0
                           then m { l = reverse (take (n-1) is) ++ [(ins,pc+1)] ++ l
                                  , r = drop n r
                                  }
                           else m { l = drop (-n) l
                                  , r = reverse (take (-n) l) ++ [(ins,pc+1)] ++ tail r
                                  }

patch :: Program -> [Program]
patch = go []
  where
    go :: Program -> Program -> [Program]
    go prefix (ins@(NOP n):is) = (prefix ++ [JMP n] ++ is) : go (prefix ++ [ins]) is
    go prefix (ins@(ACC n):is) =                             go (prefix ++ [ins]) is
    go prefix (ins@(JMP n):is) = (prefix ++ [NOP n] ++ is) : go (prefix ++ [ins]) is
    go prefix _                = []

run :: Machine -> ReturnCode
run m@Machine{..} =
  case r of
    []      -> Terminated state
    (_,0):_ -> run (step m)
    (_,1):_ -> InfiniteLoop state
    _       -> error "Out of bounds"

main :: IO ()
main = do
  input <- parse parseInput "input.txt" <$> T.readFile "input.txt"

  case input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right program -> do
      putStrLn $ "(1) " ++ show (run . initMachine $ program)

      putStr "(2) "
      forM_ (patch program) $ \p ->
        let exit = run (initMachine p)
        in case exit of
          InfiniteLoop _ -> return ()
          Terminated   _ -> print exit
