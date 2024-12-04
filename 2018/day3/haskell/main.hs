{-# Language OverloadedStrings #-}

module Main where

import           Data.Attoparsec.Text
import           Data.Either
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.IO

data Rectangle =
  Rect { rectLeftOffset :: Int
       , rectTopOffset  :: Int
       , rectWidth      :: Int
       , rectHeight     :: Int }
  deriving (Eq, Show) -- Ord?

data CoordRectangle =
  CoordRect { cRectX0 :: Int
            , cRectY0 :: Int
            , cRectX1 :: Int
            , cRectY1 :: Int }
  deriving (Eq, Show) -- Ord?

data Range =
  Range Int Int
  deriving (Eq, Show)

data Tile = Empty
          | Covered
          | Overlapped
          deriving (Eq, Show)

data Claim =
  Claim { claimId   :: Int
        , claimRect :: CoordRectangle }
  deriving (Eq, Show)

type Coord = (Int, Int)
type Table = Map Coord Tile

toCoordRect :: Rectangle -> CoordRectangle
toCoordRect (Rect x0 y0 w h) = CoordRect x0 y0 (x0 + w - 1) (y0 + h - 1)

readRect :: Parser Rectangle
readRect = Rect <$> decimal
           <*> (char ',' *> decimal)
           <*> ((char ':' *> skipSpace) *> decimal)
           <*> (char 'x' *> decimal)

readClaim :: Parser Claim
readClaim = Claim <$> (char '#' *> decimal)
            <* (skipSpace *> char '@' *> skipSpace)
            <*> (toCoordRect <$> readRect)

maybeCoordRect :: Maybe Range -> Maybe Range -> Maybe CoordRectangle
maybeCoordRect (Just (Range x0 x1)) (Just (Range y0 y1)) =
            Just (CoordRect x0 y0 x1 y1)
maybeCoordRect _ _ = Nothing

overlapRange :: Int -> Int -> Int -> Int -> Maybe Range
overlapRange a0 a1 b0 b1 = let v1 = max a0 b0
                               v2 = min a1 b1
                           in if v1 > v2
                              then Nothing
                              else Just (Range v1 v2)

overlapRect :: CoordRectangle -> CoordRectangle -> Maybe CoordRectangle
overlapRect a b = let (CoordRect ax0 ay0 ax1 ay1) = a
                      (CoordRect bx0 by0 bx1 by1) = b
                      horiRange = overlapRange ax0 ax1 bx0 bx1
                      vertiRange = overlapRange ay0 ay1 by0 by1
                  in maybeCoordRect horiRange vertiRange

tableSize :: [Claim] -> Coord
tableSize = go 0 0
  where
    go maxX maxY [] = (maxX, maxY)
    go maxX maxY (head:rest) =
      let (Claim _ (CoordRect _ _ maxX' maxY')) = head
      in go (max maxX maxX') (max maxY maxY') rest

getCell :: Table -> Coord -> Tile
getCell t c = M.findWithDefault Empty c t

coverCell :: Coord -> Table -> Table
coverCell = M.alter alter
  where
    alter Nothing = Just Covered
    alter (Just _) = Just Overlapped

getCoords :: CoordRectangle -> [Coord]
getCoords (CoordRect x0 y0 x1 y1) =
  [(x, y) | x <- [x0..x1], y <- [y0..y1]]

addToTable :: Table -> Claim -> Table
addToTable t c = let (Claim _ rect) = c
                     coords = getCoords rect
                 in foldl' (flip coverCell) t coords

findNoneOverlapping :: Table -> [Claim] -> Maybe Int
findNoneOverlapping t = let overlapped = (Overlapped ==)
                            cells (Claim _ rect)= map (getCell t) (getCoords rect)
                            matches c = not (any overlapped (cells c))
                            go [] = Nothing
                            go (claim:rest) =
                              if matches claim
                              then Just (claimId claim)
                              else go rest
                        in go

main :: IO ()
main = do
  h <- openFile "../input.txt" ReadMode
  contents <- TIO.hGetContents h
  let lines = T.lines contents
  let claims = rights (map (parseOnly readClaim) lines)
  -- let (width, height) = tableSize claims
  let table = foldl' addToTable M.empty claims
  -- print table
  putStr "Solution to part 1: "
  print ((length . filter (\(_, t) -> t == Overlapped) . M.toList) table)
  putStr "Solution to part 2: "
  maybe (TIO.putStrLn "None found") print (findNoneOverlapping table claims)
