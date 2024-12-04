module Main where

import Data.Char (digitToInt, isDigit)
import Data.Maybe (catMaybes)

data Part = Part1 | Part2

adder :: Char -> Char -> Maybe Int
adder a b = if a == b
  then Just $ digitToInt a
  else Nothing

solve :: Part -> String -> Int
solve part digits = case part of
  Part1 -> go 1 digits
  Part2 -> go ((length digits) `div` 2) digits
  where
    go :: Int -> String -> Int
    go offset digits = sum $ catMaybes duplicates
      where
        duplicates :: [Maybe Int]
        duplicates = circularZip adder digits
        circularZip :: (a -> a -> b) -> [a] -> [b]
        circularZip f as = zipWith f as (drop offset (cycle as))

main :: IO ()
main = readFile input >>= putStrLn.show.(solve Part2).(filter isDigit)
  where
    input :: String
    input = "../input"
