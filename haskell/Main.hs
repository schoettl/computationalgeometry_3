module Main where

import qualified Data.Map.Lazy as Map
import Data.List
import CG.Basic
import CG.Intersect (intersect)

main = do
  input <- getContents
  print $ countIntersects $ convertInput input

convertInput :: String -> [Line]
convertInput input = let numbers = map (read :: String -> Double) $ words input in
  if length numbers `mod` 4 /= 0 then
    error "there must be exactly 4 numbers per line"
  else
    (pairNeighbors . (map tupleToPoint) . pairNeighbors) numbers

pairNeighbors :: [a] -> [(a, a)]
pairNeighbors list = fst $ foldr accumulateToPairs ([], Nothing) list
    where
        accumulateToPairs :: a -> ([(a, a)], Maybe a) -> ([(a, a)], Maybe a)
        accumulateToPairs x (l, Nothing) = (l, Just x)
        accumulateToPairs x (l, Just y) = ((x, y):l, Nothing)

countIntersects :: [Line] -> Int
countIntersects ls = length $ processEventQueue (initialEventQueue ls) [] []

type EventQueue = Map.Map Double Event
type Intersections = [Event] -- reduzierbar auf Intersection Line Line? oder besser eigener Datentyp, der auch Schnittpunkt enthält?
type LineYOrder = [Line]

data Event = Startpoint Line | Endpoint Line | Intersection Line Line

initialEventQueue :: [Line] -> EventQueue
initialEventQueue ls = Map.fromList $ foldr f [] $ map sortLinePointsByX ls
                           where f l@(p, q) a = (xCoord p, Startpoint l):(xCoord q, Endpoint l):a

sortLinePointsByX :: Line -> Line
sortLinePointsByX (p, q) = if xCoord p < xCoord q
                             then (p, q)
                             else (q, p)

popEvent = Map.minView

processEventQueue :: EventQueue -> LineYOrder -> Intersections -> Intersections
processEventQueue es yo is
    | Map.null es = is
    | otherwise = let Just (e, es') = popEvent es
                      (es'', yo', is') = handleEvent e es' yo
                  in  processEventQueue es'' yo' is'

-- unfinished:
handleEvent :: Event -> EventQueue -> LineYOrder -> (EventQueue, LineYOrder, Intersections)
handleEvent (Startpoint l)     es yo = (es, insertStartpointInY yo l, [])
handleEvent (Endpoint l)       es yo = (es, removeEndpointFromY yo l, [])
handleEvent (Intersection a b) es yo = (es, swapIntersectionsInY yo a b, [])

insertStartpointInY :: LineYOrder -> Line -> LineYOrder
insertStartpointInY yo l = insertBy yOrder l yo
                             where yOrder a b = compare y1 y2
                                    where y1 = yCoord $ fst a
                                          y2 = yCoord $ fst b
                                    
removeEndpointFromY :: LineYOrder -> Line -> LineYOrder
removeEndpointFromY yo l = delete l yo

swapIntersectionsInY :: LineYOrder -> Line -> Line -> LineYOrder
swapIntersectionsInY yo a b = map (\x -> if x == a then b else
                                         if x == b then a else
                                                        x) yo
