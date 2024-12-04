module Main where

import Data.Char (isDigit)

import Data.Text (Text(..))
import qualified Data.Text as T (null, pack, split, unpack)

maxDiff :: [Int] -> Int
maxDiff [] = 0
maxDiff (a:as) = go a a as
  where
    go :: Int -> Int -> [Int] -> Int
    go minV maxV [] = maxV - minV
    go minV maxV (a:as) = go (min minV a) (max maxV a) as

readRows :: Text -> [[Int]]
readRows = map (collectInts.(T.split (=='\t'))) . getRows
  where
    collectInts :: [Text] -> [Int]
    collectInts = map read . filter (all isDigit) . map T.unpack
    getRows :: Text -> [Text]
    getRows = filter (not.T.null) . T.split (=='\n')

main :: IO ()
main = putStrLn.show.sum.(map maxDiff).readRows.T.pack =<< readFile "../input"
