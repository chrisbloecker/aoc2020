#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

module Main
  where
--------------------------------------------------------------------------------
import Data.CircularList
import Data.List               (elem)
import Data.Map                (Map, (!))
import Data.Maybe              (fromJust)
--------------------------------------------------------------------------------
import qualified Data.Map as M
--------------------------------------------------------------------------------

{-# INLINABLE dest #-}
dest :: Int -> Int -> [Int] -> Int
dest current len xs =
  let next = if current == 1 then len else current - 1
  in if next `elem` xs then dest next len xs else next

move1 :: Int -> [Int] -> [Int]
move1 n xs = go n (length xs) (fromList xs)
  where
    go :: Int -> Int -> CList Int -> [Int]
    go 0     _   xs = drop 1 . toList . fromJust . rotateTo 1 $ xs
    go steps len xs = let ys@[a,b,c] = take 3 . tail . toList $ xs
                          current    = fromJust . focus $ xs
                          next       = dest current len ys
                      in go (steps-1) len
                       . rotR . fromJust . rotateTo current
                       . insertL c . insertL b . insertL a -- insert the three elements
                       . fromJust . rotateTo next          -- focus on the destination, it always exists
                       . removeR . removeR . removeR       -- remove 3 elements
                       . rotR                              -- focus on the first element to the right
                       $ xs

move2 :: Int -> Map Int Int -> (Int, Map Int Int)
move2 current m = let a  = m ! current                     -- first,
                      b  = m ! a                           -- second, and
                      c  = m ! b                           -- third element to move
                      d  = m ! c                           -- next cup
                      l  = dest current (M.size m) [a,b,c] -- target
                      r  = m ! l                           -- right of target
                      m' = M.insert c       r              -- new map
                         . M.insert l       a
                         . M.insert current d
                         $ m
                  in (d, m')

main :: IO ()
main = do
  let input1 = [7,8,4,2,3,5,9,1,6]
  putStrLn $ "(1) " ++ concatMap show (move1 100 input1)

  let input2 = input1 ++ [10 .. 1000000]
      m      = M.fromList $ (1000000 : input2) `zip` input2
      m'     = snd
             . (foldr (.) id . replicate 10000000) (uncurry move2)
             $ (head input2, m)
      a      = m' ! 1
      b      = m' ! a
  putStrLn $ "(2) " ++ show (a * b)
