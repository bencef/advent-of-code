module Main where

{-
Rule for perfect squares:

d 1^2 = 0

d 2^2 = d 1^2 + 1

d 3^2 = d 2^2 + 1

so:

d 1 = 0
d n = d ((sqrt n)-1)^2 + 1

making

d n = (sqrt n) - 1

for non squares:

Up to every perfect square we add two sqrt n long sides
like this:

5
6
7 8 9

16 15 14 13
         12
         11
         10

the distance is maximized at the inbetween corner

so out of [5..9] 7 is the farthest
and out of [10..16] 13 is the farthest

The distance between consecutive numbers only changes by one.

so the distances compared to the perfect square:

 0
-1
 0 -1 0

0 -1  0 +1
         0
        -1
         0

so we start to walk from either end and subtract until halfway
then increment
-}

steps :: Integer -> Integer
steps n = case n - (squareRoot n ^! 2) of
  0 -> d n -- perfect square
  1 -> d (n-1) + 1 -- opposite to perfect square
  _ -> walkTo n
  where
    d :: Integer -> Integer
    d = pred . squareRoot

walkTo :: Integer -> Integer
walkTo n = walk start subtractingSteps stepsToSquare
  where
    walk :: Integer -> Integer -> Integer -> Integer
    walk from subtracting result = if from == n
      then result
      else walk (closer from)
           (pred subtracting)
           (result + if subtracting > 0 then -1 else 1)
    subtractingSteps :: Integer
    subtractingSteps = side `div` 2 - if even side then 1 else 0
    side :: Integer
    side = squareRoot n + 1
    squareOfSide :: Integer
    squareOfSide = square side
    stepsToSquare :: Integer
    stepsToSquare = steps squareOfSide
    closer :: Integer -> Integer
    closer = if closerToTop then pred else succ
    start :: Integer
    start = if closerToTop then squareOfSide else (square (side-1))+1
    closerToTop :: Bool
    closerToTop = squareOfSide - n < side

square :: Integer -> Integer
square n = n * n

-- from https://wiki.haskell.org/Generic_number_type#squareRoot
(^!) :: Num a => a -> Int -> a
(^!) x n = x^n
 
squareRoot :: Integer -> Integer
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^!2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r^!2 <= n && n < (r+1)^!2
   in  head $ dropWhile (not . isRoot) iters

main :: IO ()
main = putStrLn.show.steps.read =<< readFile "../../input"
