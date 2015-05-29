module CG.Basic
  ( Point (..)
  , Polygon
  , Line
  , tupleToPoint
  , addPoints
  , slope
  , intercept
  ) where

data Point = Point { xCoord::Double, yCoord::Double } deriving (Show, Eq)

type Polygon = [Point]
type Line = (Point, Point)

tupleToPoint :: (Double, Double) -> Point
tupleToPoint (x, y) = Point { xCoord = x, yCoord = y }

addPoints :: Point -> Point -> Point
addPoints p q = Point (xCoord p + xCoord q) (yCoord p + yCoord q)

slope :: Line -> Double
slope (a, b) = (yCoord b - yCoord a) / (xCoord b - xCoord a)

intercept :: Line -> Double
intercept l@(a, _) = let m = slope l in yCoord a - m * xCoord a
