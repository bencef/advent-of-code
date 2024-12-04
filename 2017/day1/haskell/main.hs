module Main where

import Data.Char (digitToInt)
import Data.Maybe (isJust, fromJust)

adder :: Char -> Char -> Maybe Int
adder a b = if a == b
  then Just $ 2 * digitToInt a
  else Nothing

solve :: String -> Integer
solve digits = sum $
               map (toInteger.fromJust) $
               filter isJust maybeDoubles
  where
    maybeDoubles :: [Maybe Int]
    maybeDoubles = circularZip adder digits
    circularZip :: (a -> a -> b) -> [a] -> [b]
    circularZip f as = zipWith f as (tail as)

result :: String -> Integer
result = solve

main :: IO ()
main = readFile input >>= putStrLn.show.solve
  where
    input :: String
    input = "../input"
