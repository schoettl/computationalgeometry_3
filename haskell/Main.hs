module Main where

import Data.List (nubBy)

main :: IO()
main = do
  input <- getContents
  print $ countIntersects $ convertInput input

convertInput :: String -> [((Double, Double), (Double, Double))]
convertInput input = let numbers = map (read :: String -> Double) $ words input in
  if length numbers `mod` 4 /= 0 then
    error "there must be exactly 4 numbers per line"
  else
    (pairNeighbors . pairNeighbors) numbers

pairNeighbors :: [a] -> [(a, a)]
pairNeighbors list = fst $ foldr accumulateToPairs ([], Nothing) list
    where
        accumulateToPairs :: a -> ([(a, a)], Maybe a) -> ([(a, a)], Maybe a)
        accumulateToPairs x (l, Nothing) = (l, Just x)
        accumulateToPairs x (l, Just y) = ((x, y):l, Nothing)

countIntersects :: (Num a, Ord a) => [((a, a), (a, a))] -> Int
countIntersects lines =
  let pairs = makePairs lines in
    length $ filter (==True) $ map intersect pairs

-- Make pairs like in C++
makePairs :: [a] -> [(a, a)]
makePairs [] = []
makePairs (x:xs) = [ (x, y) | y <- xs ] ++ makePairs xs

intersect :: (Num a, Ord a) => (((a, a), (a, a)), ((a, a), (a, a))) -> Bool
intersect (this, other)
  | ccwFst * ccwSnd > 0 = False
  | ccwFst * ccwSnd < 0 = ccw other (fst this) * ccw other (snd this) <= 0
  | ccwFst == 0 && inRect this (fst other) = True
  | ccwSnd == 0 && inRect this (snd other) = True
  | otherwise = False
  where
    ccwFst = ccw this (fst other)
    ccwSnd = ccw this (snd other)

ccw :: Num a => ((a, a), (a, a)) -> (a, a) -> a
ccw ((px, py), (qx, qy)) (x, y) = (px*qy - py*qx) + (qx*y - qy*x) + (py*x - px*y)

inRect :: (Num a, Ord a) => ((a, a), (a, a)) -> (a, a) -> Bool
inRect ((ax, ay), (bx, by)) (x, y) = inRange x (ax, bx) && inRange y (ay, by)

inRange :: (Num a, Ord a) => a -> (a, a) -> Bool
inRange x (a, b) = x >= min a b && x <= max a b
