#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

module Main
  where
--------------------------------------------------------------------------------
import Control.Monad                (forM_)
import Data.List                    (sortBy)
import Data.Maybe                   (catMaybes, isJust, fromJust)
import Data.Text                    (Text)
import Data.Void                    (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
--------------------------------------------------------------------------------
import qualified Data.Text.IO as T  (readFile)
--------------------------------------------------------------------------------

type Parser = Parsec Void Text

parseInput :: Parser (Int, [Maybe Int])
parseInput = do
  earliest <- decimal
  eol
  departures <- choice [ Just <$> decimal
                       , char 'x' >> return Nothing
                       ]
                `sepBy` char ','
  eol
  eof
  return (earliest, departures)

--------------------------------------------------------------------------------

intersect :: (Enum a, Ord a) => [a] -> [a] -> [a]
intersect xs ys = let [n0,n1] = take 2 (merge xs ys)
                  in [n0,n1..]
  where
    merge (x:xs) (y:ys) | x == y    = x : merge    xs     ys
                        | x <  y    =     merge    xs  (y:ys)
                        | otherwise =     merge (x:xs)    ys

-- yes, it should be Foldable t => (a -> a -> a) -> t a -> a
-- but I specialise it to lists
binaryFold :: (a -> a -> a) -> [a] -> a
binaryFold _ []    = error "binaryFold: empty list"
binaryFold f [x]   = x
binaryFold f [x,y] = x `f` y
binaryFold f l     = let n = length l `div` 2
                     in binaryFold f (take n l) `f` binaryFold f (drop n l)

inAll :: [(Int, Int)] -> Int -> Bool
inAll buses timepoint = all (\(bus,ix) -> (timepoint+ix) `mod` bus == 0) buses

extendedEuklid :: Int -> Int -> (Int, Int, Int)
extendedEuklid m n | m < n     = extendedEuklid n m
                   | otherwise = go m n 1 0 0 1
  where
    go r0 r1 s0 s1 t0 t1 | r1 == 0   = (r0, s0, t0)
                         | otherwise = let q = r0 `div` r1
                                       in go r1 (r0 - q*r1) s1 (s0 - q*s1) t1 (t0 - q*t1)

matchTimes :: (Int, Int) -> (Int, Int) -> (Int, Int)
matchTimes (n1, a1) (n2, a2) =
  let (_, m1, m2) = extendedEuklid n1 n2
  in ( n1*n2
     , a1*m2*n2 + a2*m1*n1
     )

main :: IO ()
main = do
  input <- parse parseInput "input.txt" <$> T.readFile "input.txt"

  case input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right (earliest, mbuses) -> do
      let buses = catMaybes mbuses
      putStrLn $ "Earliest departure: " ++ show earliest
      putStrLn $ "Available buses:    " ++ show buses

      let earliestDepartures = map (\d -> head . dropWhile (< earliest) $ [0,d..]) buses
      putStrLn $ "Earliest available: " ++ show earliestDepartures
      let (departureTime, busID) = minimum (earliestDepartures `zip` buses)
      putStrLn $ "(1) " ++ show ((departureTime - earliest) * busID)

      let generators = [ dropWhile (<0) [-ix,(fromJust mbus - ix)..]
                       | (mbus, ix) <- mbuses `zip` [0..], isJust mbus
                       ]

      putStrLn $ "(2) " ++ (show . head) (binaryFold intersect generators)
