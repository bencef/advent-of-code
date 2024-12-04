{-# Language OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Either
import           Data.Foldable
import           Data.Functor
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
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

data Shift = Shift Guard (Map Time GuardAction)
  deriving (Eq, Show)

type Line = (Date, GuardAction)

type Calendar = Map Day Shift

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

readLine :: Parser Line
readLine = (,) <$> readDate
               <* skipSpace
               <*> readGuardAction

addLineToShift :: Shift -> Line -> Shift
addLineToShift s@(Shift guard actions) (Date _ time, action) =
  case action of
    BeginShift id -> Shift (Guard id) actions
    action -> let actions' = M.insert time action actions
              in Shift guard actions'

addLineToCalendar :: Calendar -> Line -> Calendar
addLineToCalendar c l@(Date day _, _) =
  let newShift = Shift Unknown M.empty
      alter Nothing = Just (addLineToShift newShift l)
      alter (Just shift) = Just (addLineToShift shift l)
  in M.alter alter day c

main :: IO ()
main = do
  h <- openFile "../input.txt" ReadMode
  contents <- TIO.hGetContents h
  let lines = T.lines contents
  let actions = rights (map (parseOnly readLine) lines)
  let calendar = foldl' addLineToCalendar M.empty actions
  -- print calendar
  return ()
