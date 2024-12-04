{-# Language OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Functor
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.IO

data Day = Day { dayYear  :: !Int
               , dayMonth :: !Int
               , dayDay   :: !Int }
           deriving (Eq, Show, Ord)

data Time = Time { timeHours   :: !Int
                 , timeMinutes :: !Int }
            deriving (Eq, Show, Ord)

data Date = Date !Day !Time
  deriving (Eq, Show, Ord)

data GuardAction
  = BeginShift !Int
  | FallAsleep
  | WakeUp
  deriving (Eq, Show)

data Guard
  = Guard !Int
  | Unknown
  deriving (Eq, Show)

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
                <* skipSpace
                <*> readTime
                <* char ']'

readGuardAction :: Parser GuardAction
readGuardAction = let readBeginShift =
                        BeginShift <$> (string "Guard #" *> decimal)
                        <* string " begins shift"
                      readFallAsleep = string "falls asleep" $> FallAsleep
                      readWakeUp = string "wakes up" $> WakeUp
                  in readBeginShift <|> readFallAsleep <|> readWakeUp

readLine :: Parser (Date, GuardAction)
readLine = (,) <$> readDate
               <* skipSpace
               <*> readGuardAction

main :: IO ()
main = do
  h <- openFile "../input.txt" ReadMode
  contents <- TIO.hGetContents h
  let lines = T.lines contents
  mapM_ print (parseOnly readLine (lines !! 5))
  return ()
