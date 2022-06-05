import Data.List (transpose)
import Data.Char
import Data.List

-- THIS DOC CONTAINS SORTA ONE LINERS FROM CODE SIGNAL 




-- CHECK PALINDROME just reverses and checks, This could honestly be first question of one liners (that level of difficulty)
solutionPalinDrome inputString = inputString == reverse  inputString

-- zipWith (*) [1,2,3,4,5,6,7]  (tail [1,2,3,4,5,6,7])[2,6,12,20,30,42]
--solution3 s = maximum $ zipWith (*) s (tail s)

-- this makes array consecutive 
--solution s = maximum s - minimum s + 1 - length s

--
solution xs = all check $ zip xs $ tail xs where check(x1,x2) = x1<=x2


-- check largest sum of adjacent elements in array (USEFUL)
solutionAdjArrayProduct inputArray =  maximum $ zipWith (*) inputArray  (tail inputArray)


-- almost increasing sequence (HARD)
 solutionAlmostSeqeuence s = and $ (<2) . length . filter (uncurry (>=)) . zip s . tail <$> [s, tail s]


 -- cant be below a zero 
solutionNoZeroAbove = sum . map (sum . takeWhile (> 0)) . transpose

-- this is to get all longest strings that appear in a list # THIS GENERALLY SEEMS REAL USEFUL
solution10 s = filter ((==n) . length) s where n =   maximum $ map length s


-- this is to count the characters that appear in two lists.
solutionCharCount s1 s2 = length (s1 \\ (s1\\s2))




--map digitToInt $ show 1000
--[1,0,0,0]
-- checking if first half of list is equal to the second half
solutions n  = sum (take len digits ) == sum(drop len digits) where 
    digits = map digitToInt $ show n 
    len = length digits `div` 2



solutionx a = arrange (sort (filter (-1/=) a)) a

-- the sorted list [150,160,170,180] is a , a at the end is bs , itll go through b, if b == -1 input it into a 
arrange [] [] = []
arrange [] bs = bs
arrange (a:as) (b:bs)
    | b == -1 = [-1] ++ arrange (a:as) bs
    | otherwise = [a] ++ arrange as bs



-- #counting up odds and evens
-- zip will assign an integer associated with the element index , then we filter based on the second element (to only get odds / evens)
-- then we sum the first element of the tuples , do that for both and add them to a list 
solutionAlternatingSums x = [oddSum] ++ [evenSum] where
    evenSum = sum $ map fst $ filter (even . snd) $ zip x [1..] 
    oddSum =  sum $map fst $ filter (odd . snd) $ zip x [1..] 


-- not even strictly increasing, its just increasing
solutionArray (x:[]) = 0
solutionArray (x:y:xs)
  | x < y = solutionArray (y:xs)
  | otherwise = x-y+1 + solutionArray (x+1:xs)    


  -- this one is strictly increasing 
solutionStrictArray (x:[]) = 0
solutionStrictArray (x:y:xs)
  | x +1 == y = solutionStrictArray (y:xs)
  | otherwise = abs (x-y+1) + solutionStrictArray (x+1:xs) 

  -- check if palindrome exists
-- import Data.List

solutionPalindrome s = 
  length (filter odd gl) <= 1
  where
    gl = map length $ group $ sort s


-- measuring the Strongest arm (not fully correct but close)
solutionsxxxx yourLeft yourRight friendsLeft friendsRight 
    | (yourLeft+yourRight) == (friendsLeft+friendsRight) = True
    | otherwise = False


--solutionOB inputArray = (+1) $ abs $ minimum $ zipWith (-) inputArray  (tail inputArray)

--filter ( /='(')   $ filter  (/=')') "abc(dwe)"  

-- # manual digit to int without having to use any data.char library stuff
manualDigitToInt n = map (\x -> (read [x] :: Int)) (show n)
