module Ass2 (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess) where

import Data.Char (toUpper)
import Data.List
import Data.Maybe
type GameState = ([[Location]], [Location])
data Location = Location Int Int
    deriving (Eq, Show)

----- PROJECT FUNCTIONS -----

-- Takes in a co-ordinate in string form and returns it in a maybe Location form convert and check if its a valid location
-- location of form int int starting at 0 to make later maths easier
toLocation :: String -> Maybe Location
toLocation [letter, number] =
  let
    maybeX = case toUpper letter of { 'A' -> Just 0; 'B' -> Just 1; 'C' -> Just 2; 'D' -> Just 3; 'E' -> Just 4; 'F' -> Just 5; 'G' -> Just 6; 'H' -> Just 7; _ -> Nothing}
    maybeY = case number of { '1' -> Just 0; '2' -> Just 1; '3' -> Just 2; '4' -> Just 3; _ -> Nothing}
  in case (maybeX, maybeY) of
    (x, y) -> Just (Location (fromJust x) (fromJust y))
toLocation _ = Nothing
     
-- Takes in a Location and changes the ints to a string 
-- this is then used to print to the terminal in the main
fromLocation :: Location -> String
fromLocation (Location letter number) =
    let 
        x = case letter of { 0 -> 'A'; 1 -> 'B'; 2 -> 'C'; 3 -> 'D'; 4 -> 'E'; 5 -> 'F'; 6 -> 'G'; 7 -> 'H';}
        y = case number of { 0 -> '1'; 1 -> '2'; 2 -> '3'; 3 -> '4';}
    in case (x,y) of
        (x,y) -> [x,y]

-- Takes in a list of targets and list of guesses and returns 3 ints based on 
-- where the targets and guess are. Using list comprehension to check the locations with a helper function
feedback :: [Location] -> [Location] -> (Int,Int,Int)
feedback target guess = ( length( [x | x <- guess, y <- target, hit y x]),
 length ( (nub([x | x <- guess, y <- target, nearBy1 y x]) \\ [x | x <- guess, y <- target, hit y x])), 
 length ( (nub ([x | x <- guess, y <- target, nearBy2 y x]) \\ ([x | x <- guess, y <- target, hit y x] ++ [x | x <- guess, y <- target, nearBy1 y x]))))

-- This is the first guess to start the guessing it takes in a list of 3 Locations and a gamestate
-- guess is hardcoded and the game state is generated with all possible guesses and board locations
initialGuess :: ([Location],GameState)
initialGuess = ([Location 6 3, Location 4 2, Location 2 0], (generateGuesses 3 boardLocations, boardLocations))

-- Takes in an input of the last guess and game state as well as the feedback for the last guess
-- this is passed into the feedback loop function to return a new guess and update the gamestate
nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location], GameState)
nextGuess (prevGuess, prevState) (x,y,z) = (extractList((feedbackLoop prevState prevGuess (x,y,z))) , ( (feedbackLoop prevState prevGuess (x,y,z)  )))

----- HELPER FUNCTIONS CREATED FOR THE PROJECT -----

-- Takes in a target and a shot and checks if they are the same
hit :: Location -> Location -> Bool
hit target shot 
  | target == shot = True
  | otherwise =  False

-- Takes in a target and a shot and checks if they are 1 away from each other
-- on the game board 
nearBy1 :: Location -> Location -> Bool
nearBy1 target shot | target == shot = False
nearBy1 (Location x1 y1) (Location x2 y2) = abs (x1-x2) <= 1 && abs (y1-y2) <=1

-- Takes in a target and a shot and checks if they are 2 away from each other
-- on the game board 
nearBy2 :: Location -> Location -> Bool
nearBy2 target shot | target == shot = False
nearBy2 (Location x1 y1) (Location x2 y2) | abs (x1-x2) <= 1 && abs (y1-y2) <=1 = False
nearBy2 (Location x1 y1) (Location x2 y2) = abs (x1-x2) <= 2 && abs (y1-y2) <=2

-- Takes in the game state and extracts the guess for nextGuess function
extractList :: GameState -> [Location]
extractList (guess, _) = head guess

-- generates all the possible squares on the grid
boardLocations :: [Location]
boardLocations = [(Location x y)| x <- [0..7], y <- [0..3]]

-- recursive function that takes an int and list of locations to give all possible combinations of n size
-- this is used to generate every possible guess based on the grids on the board
generateGuesses :: Int -> [Location] -> [[Location]]
generateGuesses 0 lst = [[]]
generateGuesses n lst = [(x:ys) | x:xs <- tails lst, ys <- generateGuesses (n-1) xs]

-- takes an input of game state, last guess and last feedback to output a new game state
-- this loop checks for guess that result with the same feed back as got from the input guess
-- these guess are the other possible guesses, this allows the possible guesses to be shrunk significantly guess
feedbackLoop :: GameState -> [Location] -> (Int,Int,Int)  -> GameState
feedbackLoop (remainingGuesses, remainingLocs) guess (x,y,z) =  ([x2 | x2 <- remainingGuesses, feedback x2 guess == (x,y,z)], remainingLocs)


-- FILTER TESTING
filterTest :: [Location] -> [Location] -> ([Location], [Location], [Location])
filterTest ys xs =   ([x | x <- xs, y <- ys, hit y x] , nub(nub([x | x <- xs, y <- ys, nearBy1 y x])\\ [x | x <- xs, y <- ys, hit y x]) , nub (nub ([x | x <- xs, y <- ys, nearBy2 y x]) \\ ([x | x <- xs, y <- ys, hit y x] ++ [x | x <- xs, y <- ys, nearBy1 y x])))
