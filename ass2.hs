-- module Ass2 (Location, toLocation, fromLocation, feedback,
--               GameState, initialGuess, nextGuess) where

import Data.Char (toUpper)
import Data.Maybe
type GameState = ()
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


-- this is for testing to location
testCase = "F1 D2 G4"
testing :: IO ()
testing = do
   case mapM toLocation $ words testCase of

    Just target@[_,_,_] ->
      print target

     
fromLocation :: Location -> String
fromLocation (Location letter number) =
    let 
        x = case letter of { 0 -> 'A'; 1 -> 'B'; 2 -> 'C'; 3 -> 'D'; 4 -> 'E'; 5 -> 'F'; 6 -> 'G'; 7 -> 'H';}
        y = case number of { 0 -> '0'; 1 -> '1'; 2 -> '2'; 3 -> '3'; 4 -> '4'; 5 -> '5'; 6 -> '6'; 7 -> '7';}
    in case (x,y) of
        (x,y) -> [x,y]

locationToInt :: Location -> (Int,Int)
locationToInt (Location x y) = (x,y)

-- return true for a hit
hit :: Location -> Location -> Bool
hit p q 
  | p == q = True
  | otherwise =  False

-- This is true if one away
nearBy1 :: Location -> Location -> Bool
nearBy1 (Location x1 y1) (Location x2 y2) = abs (x1-x2) <= 1 && abs (y1-y2) <=1

-- True for one 2 away
nearBy2 :: Location -> Location -> Bool
nearBy2 (Location x1 y1) (Location x2 y2) = abs (x1-x2) <= 2 && abs (y1-y2) <=2

-- make grid of locations
-- grid :: Int -> Int -> Location -> [[Location]]
-- grid x y = replicate y . replicate x
-- test=grid 8 4 (Location 1 1)
-- getElementAt :: [[Location]] -> (Int, Int) -> Location
-- getElementAt test (x, y) = (test !! y) !! x


-- replace p f xs = [ if i == p then f x else x | (x, i) <- zip xs [0..] ]
-- replace2D v (x,y) = replace y (replace x (const v))
{-
import Data.Char (toUpper)
data Point = Point Int Int
data Location = Ship | Hit | Miss | Empty 
    deriving (Eq, Show)
grid :: Int -> Int -> Location -> [[Location]]
grid x y = replicate y . replicate x
test=grid 8 4 Empty
getElementAt :: [[Location]] -> (Int, Int) -> Location
getElementAt test (x, y) = (test !! y) !! x


replace p f xs = [ if i == p then f x else x | (x, i) <- zip xs [0..] ]
replace2D v (x,y) = replace y (replace x (const v))


-- toLocation :: String -> Maybe Point
-- toLocation xs = parseAsCoord xs



parseAsCoord :: String -> Maybe (Int, Int)
parseAsCoord [letter, number] =
  let
    maybeX = case toUpper letter of { 'A' -> Just 0; 'B' -> Just 1; 'C' -> Just 2; 'D' -> Just 3; 'E' -> Just 4; 'F' -> Just 5; 'G' -> Just 6; 'H' -> Just 7; _ -> Nothing }
    maybeY = case number of { '1' -> Just 0; '2' -> Just 1; '3' -> Just 2; '4' -> Just 3; _ -> Nothing }
  in case (maybeX, maybeY) of
    (Just x, Just y) -> Just (x, y)
    _ -> Nothing
parseAsCoord _ = Nothing
-}
-- import Data.Char (toUpper)
-- data Coordinate a b = Coordinate { getFirst :: a, getSecond :: b }
--     deriving (Eq, Ord, Show)

-- createCoordinate :: a -> b -> Coordinate a b
-- createCoordinate a b = Coordinate a b
-- grid2 :: Int -> Int -> [Coordinate Int Int]
-- grid2 x y = [Coordinate x y| x<- [0..x], y<-[0..y]]

-- addCoordinates :: (Num a, Num b) => Coordinate a b -> Coordinate a b -> Coordinate a b
-- addCoordinates (Coordinate a1 b1) (Coordinate a2 b2) = Coordinate (a1+a2) (b1+b2)

-- parseAsCoord :: String -> Maybe (Coordinate Int Int)
-- parseAsCoord [letter, number] =
--   let
--     maybeX = case toUpper letter of { 'A' -> 0; 'B' -> 1; 'C' -> 2; 'D' -> 3; 'E' -> 4; 'F' -> 5; 'G' -> 6; 'H' -> 7;}
--     maybeY = case number of { '1' -> 0; '2' -> 1; '3' -> 2; '4' -> 3;}
--   in case (maybeX, maybeY) of
--     (x, y) -> Just (Coordinate x y)




  --  _ -> Nothing
-- parseAsCoord _ = Nothing