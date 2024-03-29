--  File     : Main.hs
--  Author   : Peter Schachte
--  Purpose  : Test program for proj2 project to be used in Grok


module Main where

import System.Exit
import Ass2 (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess)

-- testCase = "F1 D2 G4" -- 1
-- testCase = "H3 H2 G3" -- 2
-- testCase = "A3 A2 B3" -- 3
-- testCase = "C2 E1 F3" -- 4
-- testCase = "D3 F2 E3" -- 5
-- testCase = "C3 F1 F3" -- 6
-- testCase = "H1 B2 D3" --7
testCase = "A1 D2 B3" --8

-- | Main code to test Proj2 implementations within Grok. This will be run with
-- no command line arguments, so there's no way to specify the target to search
-- for. Therefore, I've hardwired one test, but students will need to do further
-- testing on their own.
main :: IO ()
main = do
  case mapM toLocation $ words testCase of
    Just target@[_,_,_] ->
      proj2test target
    _ -> do
      putStrLn $ "toLocation Failed to convert one of " ++ testCase
                 ++ " to a Location"
      exitFailure


-- | Guess the given target, counting and showing the guesses.
proj2test :: [Location] -> IO ()
proj2test target = do
  putStrLn $ "Searching for target " ++ showLocations target
  let (guess,other) = initialGuess
  loop target guess other 1


-- | Given a target and guess and a guess number, continue guessing
-- until the right target is guessed.
loop :: [Location] -> [Location] -> Ass2.GameState -> Int -> IO ()
loop target guess other guesses = do
  putStrLn $ "Your guess #" ++ show guesses ++ ":  " ++ showLocations guess
  let answer = feedback target guess
  putStrLn $ "    My answer:  " ++ show answer
  if answer == (3,0,0)
    then do
      putStrLn $ "You got it in " ++ show guesses ++ " guesses!"
    else do
      let (guess',other') = nextGuess (guess,other) answer
      loop target guess' other' (guesses+1)


showLocations :: [Location] -> String
showLocations = unwords . (fromLocation <$>)
