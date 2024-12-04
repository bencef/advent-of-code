module Main where

import           Data.Attoparsec.Text
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.IO

data Day = Day { dayYear  :: Int
               , dayMonth :: Int
               , dayDay   :: Int }
           deriving (Eq, Show, Ord)

data Time = Time { timeHours   :: Int
                 , timeMinutes :: Int }
            deriving (Eq, Show, Ord)

data Date = Date Day Time
            deriving (Eq, Show, Ord)

readDay :: Parser Day
readDay = Day <$> decimal
              <* char '-'
              <*> decimal
              <* char '-'
              <*> decimal

readTime :: Parser Time
readTime = Time <$> decimal
                <* char ':'
                <*> decimal

readDate :: Parser Date
readDate = Date <$> (char '[' *> readDay)
                <*> (skipSpace *> readTime <* char ']')

main :: IO ()
main = do
  h <- openFile "../input.txt" ReadMode
  contents <- TIO.hGetContents h
  let lines = T.lines contents
  return ()
