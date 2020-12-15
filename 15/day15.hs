#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

module Main
  where
--------------------------------------------------------------------------------
import Data.Map                (Map)
--------------------------------------------------------------------------------
import qualified Data.Map as M (fromList, insert, lookup)
--------------------------------------------------------------------------------

mkSequence :: [Int] -> [Int]
mkSequence xs = let l = length xs
                    z = xs `zip` [1..]
                in xs ++ rest (M.fromList . take (l-1) $ z) (head . drop (l-1) $ z)
  where
    rest :: Map Int Int -> (Int, Int) -> [Int]
    rest m (n,ix) = let m' = M.insert n ix m
                    in n : case M.lookup n m of
                          Nothing  -> rest m' (     0, ix+1)
                          Just ix' -> rest m' (ix-ix', ix+1)

main :: IO ()
main = do
  let input = [12,1,16,3,11,0]
  putStrLn $ "(1) " ++ show (mkSequence input !! 2020)
  putStrLn $ "(2) " ++ show (mkSequence input !! 30000000)
