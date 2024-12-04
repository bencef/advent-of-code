module Main where

import Data.Char (digitToInt, isDigit)
import Data.Maybe (catMaybes)

data Part = Part1 | Part2

collect :: Char -> Char -> Maybe Int
collect a b = if a == b
  then Just $ digitToInt a
  else Nothing

solve :: Part -> String -> Int
solve part digits = sum duplicates
  where
    duplicates :: [Int]
    duplicates = catMaybes $ circularZip collect digits
    circularZip :: (a -> a -> b) -> [a] -> [b]
    circularZip f as = zipWith f as (drop offset (cycle as))
      where
        offset :: Int
        offset = case part of
          Part1 -> 1
          Part2 -> length digits `div` 2

main :: IO ()
main = print . solve Part2 . filter isDigit =<< readFile "../input"
