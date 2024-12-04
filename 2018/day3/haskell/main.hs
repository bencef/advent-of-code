{-# Language OverloadedStrings #-}

module Main where

import           Data.Attoparsec.Text
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

data Claim =
  Claim { claimId   :: Int
        , claimRect :: CoordRectangle }
  deriving (Eq, Show)

toCoordRect :: Rectangle -> CoordRectangle
toCoordRect (Rect x0 y0 w h) = CoordRect x0 y0 (x0 + w) (y0 + h)

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


firstOverlapTemp lines =
  do
    (Claim _ r1) <- parseOnly readClaim (lines !! 16)
    (Claim _ r2) <- parseOnly readClaim (lines !! 24)
    return (overlapRect r1 r2)

main :: IO ()
main = do
  h <- openFile "../input.txt" ReadMode
  contents <- TIO.hGetContents h
  let lines = T.lines contents
  let overlap = firstOverlapTemp lines
  print overlap
