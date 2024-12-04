module Main where

import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.IO

increaseCount :: Maybe Int -> Maybe Int
increaseCount Nothing = Just 1
increaseCount (Just val) = Just (val+1)

frequencies :: Text -> Map Char Int
frequencies = go M.empty
  where
    go :: Map Char Int -> Text -> Map Char Int
    go m rest =
      if rest == T.empty
      then m
      else let (head, tail) = fromJust (T.uncons rest)
               map = M.alter increaseCount head m
           in go map tail

processPart1 :: [Map Char Int] -> Int
processPart1 = go 0 0
  where
    go twos threes [] = twos * threes
    go twos threes (m:rest) =
      let inc shouldInc old = if shouldInc
                              then old+1
                              else old
          anyTwos = elem 2 m
          anyThrees = elem 3 m
      in go (inc anyTwos twos) (inc anyThrees threes) rest

main :: IO ()
main = do
  h <- openFile "../input.txt" ReadMode
  contents <- TIO.hGetContents h
  let lines = T.lines contents
  let freqMaps = map frequencies lines
  print (processPart1 freqMaps)
