module Main where

import Data.Char (digitToInt, isDigit)
import Data.Maybe (isJust, fromJust)

adder :: Char -> Char -> Maybe Int
adder a b = if a == b
  then Just $ digitToInt a
  else Nothing

solve :: String -> Integer
solve digits = sum $
               map (toInteger.fromJust) $
               filter isJust duplicates
  where
    duplicates :: [Maybe Int]
    duplicates = circularZip adder digits
    circularZip :: (a -> a -> b) -> [a] -> [b]
    circularZip f as = zipWith f as (drop 1 (cycle as))

main :: IO ()
main = readFile input >>= putStrLn.show.solve.(filter isDigit)
  where
    input :: String
    input = "../input"
