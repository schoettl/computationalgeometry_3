module CG.Basic
  ( Point (..)
  , Polygon
  , Line
  , tupleToPoint
  , addPoints
  ) where

data Point = Point { xCoord::Double, yCoord::Double } deriving (Show, Eq)

type Polygon = [Point]
type Line = (Point, Point)

tupleToPoint :: (Double, Double) -> Point
tupleToPoint (x, y) = Point { xCoord = x, yCoord = y }

addPoints :: Point -> Point -> Point
addPoints p q = Point (xCoord p + xCoord q) (yCoord p + yCoord q)
