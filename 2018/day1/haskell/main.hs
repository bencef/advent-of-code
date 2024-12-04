module Main where

import           Control.Monad
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import           System.IO

readNum :: Text -> Maybe Integer
readNum t = case T.uncons t of
  Nothing -> Nothing
  Just (sign, number) ->
    let num :: Integer
        num = read (T.unpack number)
    in case sign of
         '-' -> Just (-1 * num)
         '+' -> Just num
         _ -> Nothing

findDuplicate :: [Integer] -> Maybe Integer
findDuplicate = go (Set.singleton 0) 0
  where
    go _ _ [] = Nothing
    go soFar last (next:rest) =
      let new = next + last
      in if Set.member new soFar
         then Just new
         else go (Set.insert new soFar) new rest

main :: IO ()
main = do
  h <- openFile "../input.txt" ReadMode
  contents <- TIO.hGetContents h
  let lines = T.lines contents
  let nums = mapMaybe readNum lines
  mapM_ print (findDuplicate (cycle nums))
