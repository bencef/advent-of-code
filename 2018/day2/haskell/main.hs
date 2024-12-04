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

processPart2 :: [Text] -> Text
processPart2 ids = let len = T.length (head ids)
                       count m v = M.alter increaseCount v m
                       toSolution = map fst . filter (\(_, a) -> a == 2)
                       go idx = if idx == len then T.empty
                         else
                         let substitute t = T.concat [ T.take idx t
                                                     , T.singleton '_'
                                                     , T.drop (idx+1) t]
                             substitutes = map substitute ids
                             freqs = foldl' count M.empty substitutes
                             solutions = toSolution (M.toList freqs)
                         in if not (null solutions)
                            then head solutions
                            else go (idx+1)
                   in go 0

main :: IO ()
main = do
  h <- openFile "../input.txt" ReadMode
  contents <- TIO.hGetContents h
  let lines = T.lines contents
  let freqMaps = map frequencies lines
  putStr "Solution to part one: "
  print (processPart1 freqMaps)
  putStr "Solution to part two: "
  print (processPart2 lines)
