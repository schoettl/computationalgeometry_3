module CG.Intersect where

import CG.Basic

intersectPoint :: Line -> Line -> Maybe Point
intersectPoint a b = case intersectX a b of
                      Nothing -> Nothing
                      Just x -> if isInLine a && isInLine b
                                then Just Point { xCoord = x
                                                , yCoord = calcY a x}
                                else Nothing
                                where isInLine = isXInLine x

intersectX :: Line -> Line -> Maybe Double
intersectX a b = let md = slope a - slope b
                     td = intercept b - intercept a
                 in if md /= 0
                          then Just (td / md)
                          else Nothing

-- | Is x between x coordinates of start and end point?
isXInLine :: Double -> Line -> Bool
isXInLine x (a, b) = inRange x ((xCoord a), (xCoord b))

calcY :: Line -> Double -> Double
calcY l x = slope l * x + intercept l

intersect :: Line -> Line -> Bool
intersect this other
  | ccwFst * ccwSnd > 0 = False
  | ccwFst * ccwSnd < 0 = ccw other (fst this) * ccw other (snd this) <= 0
  | ccwFst == 0 && inRect this (fst other) = True
  | ccwSnd == 0 && inRect this (snd other) = True
  | otherwise = False
  where
    ccwFst = ccw this (fst other)
    ccwSnd = ccw this (snd other)

ccw :: Line -> Point -> Double
ccw
  ( Point {xCoord=px, yCoord=py}
  , Point {xCoord=qx, yCoord=qy} )
    Point {xCoord= x, yCoord= y}
  = (px*qy - py*qx) + (qx*y - qy*x) + (py*x - px*y)

inRect :: (Point, Point) -> Point -> Bool
inRect
  ( Point {xCoord=ax, yCoord=ay}
  , Point {xCoord=bx, yCoord=by} )
    Point {xCoord= x, yCoord= y}
  = inRange x (ax, bx) && inRange y (ay, by)

inRange :: (Num a, Ord a) => a -> (a, a) -> Bool
inRange x (a, b) = x >= min a b && x <= max a b
