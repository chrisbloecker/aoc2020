#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

module Main
  where
--------------------------------------------------------------------------------
import Data.CircularList
import Data.List         (elem)
import Data.Maybe        (fromJust)
--------------------------------------------------------------------------------

move :: Int -> [Int] -> [Int]
move n xs = go n (length xs) (fromList xs)
  where
    go :: Int -> Int -> CList Int -> [Int]
    go 0     _   xs = take 3 . tail . toList $ xs --drop 1 . toList . fromJust . rotateTo 1 $ xs
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

    dest :: Int -> Int -> [Int] -> Int
    dest current len xs =
      let next = if current == 1 then len else current - 1
      in if next `elem` xs then dest next len xs else next

main :: IO ()
main = do
  let input1 = [7,8,4,2,3,5,9,1,6]
  putStrLn $ "(1) " ++ concatMap show (move 100 input1)
