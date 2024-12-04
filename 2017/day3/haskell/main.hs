module Main where

data Part = Part1 | Part2

solve :: Part -> Int -> Int
solve _ = id

main :: IO ()
main = putStrLn.show.(solve Part1).read =<< readFile "../input"
