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



import Data.Char (toUpper)
import Data.Maybe
type Location2 = (Int, Int)
data Point = Point Int Int
data Location = Location Int Int
    deriving (Eq, Show)
-- make grid of locations
grid :: Int -> Int -> Location -> [[Location]]
grid x y = replicate y . replicate x
test=grid 8 4 (Location 1 1)
getElementAt :: [[Location]] -> (Int, Int) -> Location
getElementAt test (x, y) = (test !! y) !! x


replace p f xs = [ if i == p then f x else x | (x, i) <- zip xs [0..] ]
replace2D v (x,y) = replace y (replace x (const v))


--toLocation :: String -> Maybe Location
--toLocation xs = Location (1 1)
-- grid2 :: Int -> Int -> [Location] -> [[Location]]
-- grid2 x y = [Location x y| x<- [0..x], y<-[0..y]]
-- test2 = grid 8 4

parseAsCoord :: String -> Maybe Location
parseAsCoord [letter, number] =
  let
    maybeX = case toUpper letter of { 'A' -> Just 0; 'B' -> Just 1; 'C' -> Just 2; 'D' -> Just 3; 'E' -> Just 4; 'F' -> Just 5; 'G' -> Just 6; 'H' -> Just 7; _ -> Nothing}
    maybeY = case number of { '1' -> Just 0; '2' -> Just 1; '3' -> Just 2; '4' -> Just 3; _ -> Nothing}
  in case (maybeX, maybeY) of
    (x, y) -> Just (Location (fromJust x) (fromJust y))
parseAsCoord _ = Nothing

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