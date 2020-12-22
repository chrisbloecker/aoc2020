#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  where
--------------------------------------------------------------------------------
import Data.Set                     (Set, member, empty, insert)
import Data.Text                    (Text)
import Data.Void                    (Void)
import Text.Megaparsec       hiding (empty)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
--------------------------------------------------------------------------------
import qualified Data.Text.IO as T  (readFile)
import qualified Data.Set     as S  (empty, insert)
--------------------------------------------------------------------------------

type Card = Int
type Deck = [Card]

type Parser = Parsec Void Text

decksP:: Parser (Deck, Deck)
decksP = do string "Player 1:" >> eol
            deck1 <- some (decimal <* eol)
            eol
            string "Player 2:" >> eol
            deck2 <- some (decimal <* eol)
            eof
            return (deck1, deck2)

--------------------------------------------------------------------------------

rules1 :: Deck -> Deck -> (Deck, Deck)
rules1 (a:as) (b:bs) | a > b     = rules1 (as ++ [a,b])  bs
                     | a < b     = rules1 as            (bs ++ [b,a])
                     | otherwise = error "unexpected: equal cards."
rules1 d1 d2 = (d1, d2)

rules2 :: Deck -> Deck -> (Deck, Deck)
rules2 = go empty empty
  where
    go :: Set Deck -> Set Deck -> Deck -> Deck -> (Deck, Deck)
    go s1 s2 d1@(a:as) d2@(b:bs)
      | d1 `member` s1 || d2 `member` s2 = (d1, [])
      | a <= length as && b <= length bs =
          case rules2 (take a as) (take b bs) of
            (_,[]) -> go (d1 `insert` s1) (d2 `insert` s2) (as ++ [a,b])  bs
            ([],_) -> go (d1 `insert` s1) (d2 `insert` s2)  as           (bs ++ [b,a])
            p      -> error $ "no winner: " ++ show p
      | otherwise =
          case a `compare` b of
            GT -> go (d1 `insert` s1) (d2 `insert` s2) (as ++ [a,b])  bs
            LT -> go (d1 `insert` s1) (d2 `insert` s2)  as           (bs ++ [b,a])
            EQ -> error "unexpected: equal cards"
    go _ _ d1 d2 = (d1, d2)

main :: IO ()
main = do
  input <- parse decksP "input.txt" <$> T.readFile "input.txt"

  case input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right (deck1, deck2) -> do
      let winner1 = case rules1 deck1 deck2 of
            (w,  []) -> w
            ([], w ) -> w
            p        -> error $ "no winner: " ++ show p
      putStrLn $ "(1) " ++ show (sum $ zipWith (*) winner1 [length winner1, length winner1 - 1 .. 0])

      let winner2 = case rules2 deck1 deck2 of
            (w,  []) -> w
            ([], w ) -> w
            p        -> error $ "no winner: " ++ show p
      putStrLn $ "(2) " ++ show (sum $ zipWith (*) winner2 [length winner2, length winner2 - 1 .. 0])
