module CG.CG3 where

import qualified Data.Map.Lazy as Map
import Data.List
import Data.Maybe (fromJust, isJust)
import CG.Basic
import CG.Intersect (intersectPoint)

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
type Intersections = [Event]
type LineYOrder = [Line]
type LinePair = (Line, Line)

data Event = Startpoint Line | Endpoint Line | Intersection Line Line Point

initialEventQueue :: [Line] -> EventQueue
initialEventQueue ls = Map.fromList $ foldr f [] $ map sortLinePointsByX ls
                           where f l@(p, q) a = (xCoord p, Startpoint l):(xCoord q, Endpoint l):a

sortLinePointsByX :: Line -> Line
sortLinePointsByX (p, q) = if xCoord p < xCoord q
                             then (p, q)
                             else (q, p)

-- | Pop the next event from an event queue. The queue must not be empty.
popEvent es = fromJust $ Map.minView es

-- | Process events from the event queue (recursivly). Returns the list of Intersection events.
processEventQueue :: EventQueue -> LineYOrder -> Intersections -> Intersections
processEventQueue es yo is
    | Map.null es = is
    | otherwise = let (e, es') = popEvent es
                      (es'', yo', ii) = handleEvent es' yo e
                      is' = if ii then e:is else is
                  in  processEventQueue es'' yo' is'


-- | Handle an event (popped from the event queue).
handleEvent :: EventQueue -> LineYOrder -> Event -> (EventQueue, LineYOrder, Bool)
handleEvent es yo (Startpoint l)       = let yo' = insertLineInY yo l in (updateEventQueue es yo', yo', False)
handleEvent es yo (Endpoint l)         = (es, removeLineFromY yo l, False)
handleEvent es yo (Intersection a b _) = (es, swapLinesInY yo a b, True)

-- | Alle benachbarten Linien auf Schnittpunkt testen und diese als Ereignisse hinzufügen, wenn x > aktuelles x
updateEventQueue :: EventQueue -> LineYOrder -> EventQueue
updateEventQueue es yo = let linePairs = zip yo (tail yo)
                             minX = fst $ Map.findMin es
                             intersects = calculateRelevantIntersections linePairs minX
                         in  foldr insertIntersection es intersects

-- | Calculate Intersection events for line pairs, where the x coordinate of
-- the intersection point is greater than the minimum x of the event queue.
calculateRelevantIntersections :: [LinePair] -> Double -> Intersections
calculateRelevantIntersections linePairs minX = let intersects = calculateIntersects linePairs
                                                    intersects' = filter (\(_, p) -> xCoord p > minX) intersects
                                                in map (\((a, b), p) -> Intersection a b p) intersects'

-- | Calculate the intersection points for the given line pairs.
calculateIntersects :: [LinePair] -> [(LinePair, Point)]
calculateIntersects linePairs = let all = zip linePairs $ map (uncurry intersectPoint) linePairs
                                    intersects = filter (\(_, p) -> isJust p) all
                                in map (\(t, Just p) -> (t, p)) intersects

-- | Insert an event into the event queue. The event queue is sorted by x coordinates.
insertIntersection :: Event -> EventQueue -> EventQueue
insertIntersection i@(Intersection _ _ p) es = Map.insert (xCoord p) i es

insertLineInY :: LineYOrder -> Line -> LineYOrder
insertLineInY yo l = insertBy yOrder l yo
                             where yOrder a b = compare y1 y2
                                    where p  = fst a
                                          y1 = yCoord p
                                          q1 = fst b
                                          q2 = snd b
                                          m  = (yCoord q2 - yCoord q1) / (xCoord q2 - xCoord q1)
                                          y2 = m * (xCoord p - xCoord q1) + yCoord q1
                                    
removeLineFromY :: LineYOrder -> Line -> LineYOrder
removeLineFromY yo l = delete l yo

swapLinesInY :: LineYOrder -> Line -> Line -> LineYOrder
swapLinesInY yo a b = map (\x -> if x == a then b else
                                 if x == b then a else
                                                x) yo
