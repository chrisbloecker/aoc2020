#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

{-# LANGUAGE BangPatterns #-}

module Main
  where
--------------------------------------------------------------------------------

reverseLoop :: Int -> Int
reverseLoop target = go 7 1
  where go !x !loop | x == target = loop
                    | otherwise   = go (x * 7 `mod` 20201227) (loop + 1)

forwardLoop :: Int -> Int -> Int
forwardLoop subject = go 1
  where go !x !0    = x
        go !x !loop = go (x * subject `mod` 20201227) (loop - 1)

main :: IO ()
main = do
  let card = 10943862
      door = 12721030

      cardLoop = reverseLoop card
      doorLoop = reverseLoop door

  putStrLn $ show (forwardLoop card doorLoop)
