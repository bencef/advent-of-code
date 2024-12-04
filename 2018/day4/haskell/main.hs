{-# Language OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Either
import           Data.Foldable
import           Data.Functor
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
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
  deriving (Eq, Show, Ord)

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

nextDay :: Day -> Day
nextDay (Day year 1     31)  = Day year     2     1
nextDay (Day year 2     28)  = Day year     3     1 -- no leap year check
nextDay (Day year 3     31)  = Day year     4     1
nextDay (Day year 4     30)  = Day year     5     1
nextDay (Day year 5     31)  = Day year     6     1
nextDay (Day year 6     30)  = Day year     7     1
nextDay (Day year 7     31)  = Day year     8     1
nextDay (Day year 8     31)  = Day year     9     1
nextDay (Day year 9     30)  = Day year     10    1
nextDay (Day year 10    31)  = Day year     11    1
nextDay (Day year 11    30)  = Day year     12    1
nextDay (Day year 12    31)  = Day (year+1) 1     1
nextDay (Day year month day) = Day year     month (day+1)

addLineToCalendar :: Calendar -> Line -> Calendar
addLineToCalendar c l@(Date day (Time hour _), _) =
  let newShift = Shift Unknown M.empty
      alter Nothing = Just (addLineToShift newShift l)
      alter (Just shift) = Just (addLineToShift shift l)
      dayOfShift = if hour == 23
                   then nextDay day
                   else day
  in M.alter alter dayOfShift c

calcMinutes :: Int -> Int -> Int
calcMinutes fellAsleep wokeUp = wokeUp - fellAsleep

sleptInShift :: Shift -> Int
sleptInShift (Shift _ m) = go 0 (WakeUp,0) (M.toList m)
  where
    go acc _ [] = acc
    go acc last ((Time _ now, action) : rest) =
      case last of
        (FallAsleep, when) ->
          go (acc + calcMinutes when now) (WakeUp, now) rest
        (WakeUp, _) ->
          go acc (FallAsleep, now) rest

collectMinutes :: Map Guard Int -> Shift -> Map Guard Int
collectMinutes m s@(Shift guard _) =
  let minutes = sleptInShift s
      alter Nothing = Just minutes
      alter (Just minutes') = Just (minutes + minutes')
  in M.alter alter guard m

sleptMinutes :: Calendar -> Map Guard Int
sleptMinutes = foldl' collectMinutes M.empty

sleptRange :: Int -> Int -> [Int]
sleptRange start end = [start..(end-1)]

sleptTheMost :: Map Guard Int -> Guard
sleptTheMost m = go (Unknown, 0) (M.toList m)
  where
    go g@(_, best) ((guard, minutes) : rest) =
      if minutes > best
      then go (guard, minutes) rest
      else go g rest
    go (guard, _) [] = guard

filterFor :: Guard -> Calendar -> Calendar
filterFor g = M.filter (\(Shift guard _) -> g == guard)

-- TODO: refactor common parts with sleptInShift
minutesSpentSleeping :: Map Time Int -> Shift -> Map Time Int
minutesSpentSleeping map0 (Shift _ m) = go map0 (WakeUp,0) (M.toList m)
  where
    go acc _ [] = acc
    go acc last ((Time _ now, action) : rest) =
      case last of
        (FallAsleep, when) ->
          go (add acc when now) (WakeUp, now) rest
        (WakeUp, _) ->
          go acc (FallAsleep, now) rest
    add m w n = let minutes = sleptRange w n
                    alter Nothing = Just 1
                    alter (Just val) = Just (val+1)
                    add1 m k = M.alter alter (Time 0 k) m
                in foldl' add1 m minutes

keyWithMaxVal :: Ord a => Map k a -> k
keyWithMaxVal m = go Nothing (M.toList m)
  where
    go (Just (k, _)) [] = k
    go acc@(Just (_, old)) ((k, new) : rest) =
      if new > old
      then go (Just (k, new)) rest
      else go acc rest
    go Nothing (head : rest) =
      go (Just head) rest
    go _ _ = error "no values"

getId :: Guard -> Maybe Int
getId (Guard id) = Just id
getId _ = Nothing

getGuards :: Calendar -> Set Guard
getGuards c = let collect acc (Shift guard _) = S.insert guard acc
              in foldl' collect S.empty c

main :: IO ()
main = do
  h <- openFile "../input.txt" ReadMode
  contents <- TIO.hGetContents h
  let lines = T.lines contents
  let actions = rights (map (parseOnly readLine) lines)
  let calendar = foldl' addLineToCalendar M.empty actions
  let sleepiestGuard = (sleptTheMost . sleptMinutes) calendar
  let calendarForSleepy = filterFor sleepiestGuard calendar
  let sleepyMinuteMap = foldl' minutesSpentSleeping M.empty calendarForSleepy
  let sleepiestTime@(Time _ sleepiestMinute) = keyWithMaxVal sleepyMinuteMap
  TIO.putStr "sleepiest guard: "
  print sleepiestGuard
  TIO.putStr "sleepiest minute: "
  print sleepiestMinute
  TIO.putStr "times they slept in that minute: "
  print (sleepyMinuteMap M.! sleepiestTime)
  TIO.putStr "solution to part 1: "
  maybe (TIO.putStrLn "No solution found")
    (\id -> print (id * sleepiestMinute))
    (getId sleepiestGuard)
  -- End of Part 1
  let allGuards = getGuards calendar
  TIO.putStr "all guards: "
  print allGuards
  return ()
