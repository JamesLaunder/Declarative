module Ass2 (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess) where

import Data.Char (toUpper)
import Data.List
import Data.Maybe
type GameState = [[Location]]
data Location = Location Int Int
    deriving (Eq, Show)

toLocation :: String -> Maybe Location
toLocation [letter, number] =
  let
    maybeX = case toUpper letter of { 'A' -> Just 0; 'B' -> Just 1; 'C' -> Just 2; 'D' -> Just 3; 'E' -> Just 4; 'F' -> Just 5; 'G' -> Just 6; 'H' -> Just 7; _ -> Nothing}
    maybeY = case number of { '1' -> Just 0; '2' -> Just 1; '3' -> Just 2; '4' -> Just 3; _ -> Nothing}
  in case (maybeX, maybeY) of
    (x, y) -> Just (Location (fromJust x) (fromJust y))
toLocation _ = Nothing
     
fromLocation :: Location -> String
fromLocation (Location letter number) =
    let 
        x = case letter of { 0 -> 'A'; 1 -> 'B'; 2 -> 'C'; 3 -> 'D'; 4 -> 'E'; 5 -> 'F'; 6 -> 'G'; 7 -> 'H';}
        y = case number of { 0 -> '1'; 1 -> '2'; 2 -> '3'; 3 -> '4';}
    in case (x,y) of
        (x,y) -> [x,y]

-- return true for a hit
hit :: Location -> Location -> Bool
hit p q 
  | p == q = True
  | otherwise =  False

-- This is true if one away
nearBy1 :: Location -> Location -> Bool
nearBy1 p q | p == q = False
nearBy1 (Location x1 y1) (Location x2 y2) = abs (x1-x2) <= 1 && abs (y1-y2) <=1

-- True for one 2 away
nearBy2 :: Location -> Location -> Bool
nearBy2 p q | p == q = False
nearBy2 (Location x1 y1) (Location x2 y2) | abs (x1-x2) <= 1 && abs (y1-y2) <=1 = False
nearBy2 (Location x1 y1) (Location x2 y2) = abs (x1-x2) <= 2 && abs (y1-y2) <=2

-- FILTER TESTING
filterTest :: [Location] -> [Location] -> ([Location], [Location], [Location])
filterTest ys xs =   ([x | x <- xs, y <- ys, hit y x] , nub(nub([x | x <- xs, y <- ys, nearBy1 y x])\\ [x | x <- xs, y <- ys, hit y x]) , nub (nub ([x | x <- xs, y <- ys, nearBy2 y x]) \\ ([x | x <- xs, y <- ys, hit y x] ++ [x | x <- xs, y <- ys, nearBy1 y x])))

-- Feedback working 100% now
feedback :: [Location] -> [Location] -> (Int,Int,Int)
feedback ys xs = ( length( [x | x <- xs, y <- ys, hit y x]),
 length ( nub(nub([x | x <- xs, y <- ys, nearBy1 y x])\\ [x | x <- xs, y <- ys, hit y x])), 
 length (nub (nub ([x | x <- xs, y <- ys, nearBy2 y x]) \\ ([x | x <- xs, y <- ys, hit y x] ++ [x | x <- xs, y <- ys, nearBy1 y x]))))

initialGuess = ([Location 0 1, Location 0 2, Location 0 0], locs) -- this is the initial guess

-- This kinda works
-- it iterates properly but it doesnt guess every possible combination cus im dumb so it never gets it right
nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location], GameState)
nextGuess (xs, ys) (0,0,0) = (ys !! ((length xs) `div` 3), squares xs)
nextGuess (xs, ys) (_,_,_) = (ys !! ((length xs) `div` 3), drop 1 ys)


locs :: [[Location]]
locs = combos 3 allLocs

allLocs :: [Location]
allLocs = [(Location x y)| x <- [0..7], y <- [0..3]]

squares :: [Location] -> [[Location]]
squares xs = combos 3 (squaresAvailable xs)

squaresAvailable :: [Location] -> [Location]
squaresAvailable xs = [(Location x y)| x <- [0..7], y <- [0..3], (Location x y) /= head xs && (Location x y) /= last xs && (Location x y) /= (xs !! 1)]

combos :: Int -> [Location] -> [[Location]]
combos 0 lst = [[]]
combos n lst = [(x:ys) | x:xs <- tails lst, ys <- combos (n-1) xs]



