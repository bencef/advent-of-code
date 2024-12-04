module Main where

maxDiff :: [Int] -> Int
maxDiff [] = 0
maxDiff (a:as) = go a a as
  where
    go :: Int -> Int -> [Int] -> Int
    go minV maxV [] = maxV - minV
    go minV maxV (a:as) = go (min minV a) (max maxV a) as

main :: IO ()
main = putStrLn.show $ maxDiff [2, 3, 7, 10]
