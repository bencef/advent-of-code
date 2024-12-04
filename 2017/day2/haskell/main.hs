module Main where

import Data.Char (isDigit)

import Data.Text (Text(..))
import qualified Data.Text as T (null, pack, split, unpack)

data Part = Part1 | Part2

maxDiff :: [Int] -> Int
maxDiff [] = 0
maxDiff (a:as) = go a a as
  where
    go :: Int -> Int -> [Int] -> Int
    go minV maxV [] = maxV - minV
    go minV maxV (a:as) = go (min minV a) (max maxV a) as

evenDivs :: [Int] -> Int
evenDivs as = get [ i `div` j | i <- as, j <- as, i `mod` j == 0]
  where
    get [] = 0
    get (1:as) = get as
    get (a:as) = a

readRows :: Text -> [[Int]]
readRows = map (collectInts.(T.split (=='\t'))) . getRows
  where
    collectInts :: [Text] -> [Int]
    collectInts = map read . filter (all isDigit) . map T.unpack
    getRows :: Text -> [Text]
    getRows = filter (not.T.null) . T.split (=='\n')

solve :: Part -> [[Int]] -> Int
solve part = sum . map solver
 where
   solver = case part of
     Part1 -> maxDiff
     Part2 -> evenDivs

main :: IO ()
main = putStrLn.show.(solve Part2).readRows.T.pack =<< readFile "../input"
