import Data.List
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as Map 
import Data.Char
import Data.Char (isDigit, digitToInt) 
import Control.Applicative
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import System.IO (hFlush, stdout)
import Debug.Trace



-- BASIC USEFUL FUNCTIONS 

g :: Int -> (Int -> Int)
g x y = x * y

fib :: Int -> Int
-- define fib function here
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n-2)

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)

fact2 :: Integer -> Integer
fact2 n
    | n < 0     = 0
    | n == 0    = 1
    | otherwise = n * fact2 (n-1)


fib2 :: Int -> Int
-- define fib function here
fib2 n
   | n == 0 = 0
   | n == 1 = 1
   | n > 1 = fib2 (n-1) + fib2 (n-2)
   | otherwise = 0

leap :: Int -> Bool
leap year
    | year `mod` 4   /= 0 = False
    | year `mod` 100 /= 0 = True
    | year `mod` 400 /= 0 = False
    | otherwise           = True


xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True 
xor False True = True 
xor False False = False


-- Define your myHead and myTail functions here
myHead :: [t] -> t
myHead (x:xs) = x

myTail :: [t] -> [t]
myTail (x:xs) = xs


-- Define your append function here
append :: [t] -> [t] -> [t]
append [] [] = []
append (x:xs) [] = (x:xs)
append [] (y:ys) = (y:ys) 
append (x:xs) (y:ys) = (x:xs) ++ (y:ys)


myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

evens :: Int -> [Int]
evens n = [2,4..2*n]

search :: Int -> [Int] -> Bool
search x [] = False
search x (y:ys)
    | x == y    = True
    | otherwise = search x ys


bsearch :: Int -> [Int] -> Bool
bsearch x [] = False
bsearch x ys
    | x  < mid = bsearch x (take m ys)
    | x == mid = True
    | x  > mid = bsearch x (drop (m+1) ys)
    where m = (length ys) `div` 2
          mid = ys !! m


qsort :: [Int] -> [Int]
qsort [] = []
qsort (pivot:others) = (qsort lowers) ++ [pivot] ++ (qsort highers)
    where lowers  = filter (<pivot)  others
          highers = filter (>=pivot) others


msort :: [Int] -> [Int]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort as) (msort bs)
    where as = take n xs
          bs = drop n xs
          n  = (length xs) `div` 2

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y     = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys


freqs :: [Int] -> [Int]
freqs lst = map length (group (sort lst))


commonCharacterCount s1 s2 = length (s1 \\ (s1 \\ s2))


sortByHeight a = go a (sort [x | x <- a, x /= (-1)])
    where
        go [] _ = []
        go xs [] = xs
        go (x:xs) (y:ys) = if x == (-1) then x : go xs (y:ys) else y : go xs ys


reverseInParentheses :: String -> String
reverseInParentheses s = rev s [] where
    rev [] stk = reverse stk
    rev (')':t) stk = let (s1, s2) = span (/='(') stk in rev t (reverse s1 ++ tail s2)
    rev (h:t) stk = rev t (h:stk)

-- list Comprehension 

a = [10 * a + b | a <- [1..10], b <- [1..10], a < b]

myMap :: (a -> b) -> [a] -> [b]
-- define your function here!
myMap f xs = [f x | x <- xs]

myFilter :: (a -> Bool) -> [a] -> [a]
-- define your function here!
myFilter f xs = [x | x <- xs, f x]

peter = [ x++" "++y
    | x <- ["hello", "fly away", "come back"]
    , length x > 7
    , y <- ["peter", "matthew", "paul"]
    , length y < 7
    ]

matches :: Eq a => [a] -> [a] -> Int
matches xs ys = length $ [ x | x <- xs, y <- ys,(x==y)]


dot xs ys = sum [x*y | (x, y) <- zip xs ys]

complementary :: Char -> Char -> Bool
complementary x y
    | (x,y) == ('A','T') = True
    | (x,y) == ('T','A') = True
    | (x,y) == ('C','G') = True
    | (x,y) == ('G','C') = True
    | otherwise = False


data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     } deriving (Eq, Show)

ben = Person {firstName = "Ben", lastName = "Tebbutt", age = 24}


type PhoneNumber = String  
type Name = String  
type PhoneBook = [(Name,PhoneNumber)]  

phoneBook =      
    [("betty","555-2938")     
    ,("bonnie","452-2928")     
    ,("patsy","493-2928")     
    ,("lucille","205-2928")     
    ,("wendy","939-8282")     
    ,("penny","853-2492")     
    ]  

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool  
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook  


-- Map with capital M is the look up map. 

data LockerState = Taken | Free deriving (Show, Eq)  
  
type Code = String  
  
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code  
lockerLookup lockerNumber map =   
    case Map.lookup lockerNumber map of   
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"  
        Just (state, code) -> if state /= Taken   
                                then Right code  
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"  

lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]


-- Using Flip such that we don't need to use a clunky lambda
-- for example any(\ x ->  elem x "aeiou") xs  

checkVowelExists :: String -> Bool
checkVowelExists xs = any (flip elem "aeiou") xs 


zWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zWith f [] _ = []
zWith f _ [] = []
zWith f (x:xs) (y:ys) = f x y : zWith f xs ys


-- CHESS PIECE  

data ChessPiece = ChessPiece PieceColour PieceRank

data PieceColour = Black | White

data PieceRank = King | Queen | Rook | Bishop | Knight | Pawn

instance Show ChessPiece where
    show (ChessPiece c r) = show c ++ show r

instance Show PieceColour where
    show Black = "B"
    show White = "W"

instance Show PieceRank where
    show King   = "K"
    show Queen  = "Q"
    show Rook   = "R"
    show Bishop = "B"
    show Knight = "N"
    show Pawn   = "P"

toChessPiece :: String -> Maybe ChessPiece
toChessPiece str =
    case filter (/=' ') str of
      [c,r] -> do
        colour <- toPieceColour c
        rank   <- toPieceRank r
        return $ ChessPiece colour rank
      _ -> Nothing

toPieceColour :: Char -> Maybe PieceColour
toPieceColour 'B' = Just Black
toPieceColour 'W' = Just White
toPieceColour _   = Nothing

toPieceRank :: Char -> Maybe PieceRank
toPieceRank 'K' = Just King
toPieceRank 'Q' = Just Queen
toPieceRank 'R' = Just Rook
toPieceRank 'B' = Just Bishop
toPieceRank 'N' = Just Knight
toPieceRank 'P' = Just Pawn
toPieceRank _   = Nothing

--Implement your two versions of maybe_drop function here.
--name them maybe_drop1 and maybe_drop2

maybe_tail :: [a] -> Maybe [a]
maybe_tail [] = Nothing
maybe_tail (_:xs) = Just xs 

maybe_drop :: Int -> [a] -> Maybe [a]
maybe_drop n xs
    | n > length xs = Nothing
    | otherwise =  Just $ drop n xs 
    
maybe_drop1 :: Int -> [a] -> Maybe [a]
maybe_drop1 0 xs = Just xs
maybe_drop1 n xs 
     | n > 0 = maybe_tail xs >>= maybe_drop1 (n-1)

maybe_drop2 :: Int -> [a] -> Maybe [a]
maybe_drop2 0 xs = Just xs
maybe_drop2 n xs | n > 0 =
        let mt = maybe_tail xs in
        case mt of
                Nothing -> Nothing
                Just xs1 -> maybe_drop2 (n-1) xs1





-- really good function 
newList :: Sequence -> Sequence
newList xs = scanl1 (\acc x -> if x > acc then x else acc) xs




type Sequence = [Int] 

newList2 :: Sequence -> Bool
newList2 xs =  lengthFilter $ getLength $ group $ scanl1 (\acc x -> if x > acc then x else acc) xs

getLength :: [[Int]] -> [Int]
getLength [x] = [length x]
getLength (x:xs) = [length x] ++ getLength xs


lengthFilter :: [Int] -> Bool
lengthFilter xs 
     | length [x | x <- xs, (x > 2)] > 1 = False
     | length [x | x <- xs, (x > 3)] >= 1 =False 
     | otherwise = True


myStringScan :: [String] -> [String]
myStringScan xs =  scanl1 (\acc x -> if length x >= length acc then x else acc) xs


type Matrix = [[Int]]

matrixElementsSum :: Matrix -> Int
matrixElementsSum (x:xs) = sum $ concat $ getValidColumns $ transpose (x:xs)

getValidColumns :: Matrix -> Matrix
getValidColumns [x] = [takeWhile(>0) x]
getValidColumns (x:xs) = [takeWhile(>0) x] ++ getValidColumns xs

applicative = [(+),(*)] <*> [1,2] <*> [3,4]  

-- (*) <$> Just 2 <*> Just 8  
-- (++) <$> Just "klingon" <*> Nothing  
-- (-) <$> [3,4] <*> [1,2,3] 
-- (*) <$> [2,5,10] <*> [8,10,11]   
-- 



str_to_num :: String -> Maybe Int
str_to_num [] = Nothing
str_to_num (d:ds) = str_to_num_acc 0 (d:ds)

--Each time we get another digit we multiply the value so far by 10 and
--add the new digit. If we get to the end we return Just the value and if
--we get a non-digit we return Nothing.

str_to_num_acc :: Int -> String -> Maybe Int
str_to_num_acc val [] = Just val
str_to_num_acc val (d:ds) =
    if isDigit d then str_to_num_acc (10*val + digitToInt d) ds
    else Nothing

monad = [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)  


--FILTER 

filt1 = filter (>0) $ filter (<10) [1,2,3]

--double filter 

filt2= filter(\x -> x >= 0 && x < 10) [1,2,3]





-- FOLD

sum' :: (Num a) => [a] -> a  
sum' = foldl (+) 0   

elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys  

map' :: (a -> b) -> [a] -> [b]  
map' f xs = foldr (\x acc -> f x : acc) [] xs  

maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)  
  
reverse' :: [a] -> [a]  
reverse' = foldl (\acc x -> x : acc) []  
  
product' :: (Num a) => [a] -> a  
product' = foldr1 (*)  
  
filter' :: (a -> Bool) -> [a] -> [a]  
filter' p = foldr (\x acc -> if p x then x : acc else acc) []  
  
head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  
  
last' :: [a] -> a  
last' = foldl1 (\_ x -> x)  


--SCAN  

scan1 = scanl (+) 0 [3,5,2,1]  

scan2 = scanr (+) 0 [3,5,2,1]  

scan3 = scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]  

scan4 = scanl (flip (:)) [] [3,2,1]  




-- FUNCTORS 
-- any data type that defines how fmap applies to it 
-- fmap takes a function like (+3) and a functor like just 2 and returns a new functor like just 5



-- we can also use fmap to combine functions 
-- right side applied first 
foo = fmap (+2)(*2)
foo1 = (^2) <$> (+2)


-- APPLICATIVES 

-- the same as functors however, the in applicatives our values AND functions are wrapped in a context. For example just (+3)


foo3 = Just (+3) <*> Just 2

-- the * 2 is applied to every item in the list and then the +3 is applied to every item in the list
foo4a = [(*2), (+3)] <*> [1, 2, 3] 

foo4b = (*) <$> [1,2,3] <*> [10,100,1000]  

-- we can use fmap to create the context function 
-- (+) <$> (Just 5)
-- = Just (+5)
-- then use the applicative function on another context value
-- Just (+5) <*> (Just 3) = Just 8


-- MONADS 

-- monads apply a function that returns a wrapped value to a wrapped value 
-- Suppose half is a function that only works on even numbers: 

half x = if even x
           then Just (x `div` 2)
           else Nothing

-- if we put a value into the function it will spit out a wrapped value 
-- for example if we put in 3 , it will spit out the wrapped value of nothing
-- if we feed it a wrapped value it will reject it. 

-- we need to use >>= to shove our wrapped value into the function. 
-- it is like a hammer which forces a wrapped value into the function. 
-- for example:

foo5 = Just 3 >>= half -- == Nothing
foo6 = Just 4 >>= half -- == Just 2

-- >>= takes a monad like just3 and a function that returns a monad (like half) and returns it.


-- you can also chain these binds 

foo7a = Just 20 >>= half >>= half >>= half

foo7b = [3,4,5] >>= \x -> [x,-x]  

-- in the list monad, [] is the same as nothing 
foo7c = [1,2,3] >>= \x -> []  -- == []



addOne :: (Monad m, Num a) => a -> m a
addOne a = return (a + 1)

{-
-- List
[ ]    >>= addOne == [ ]
[1]    >>= addOne == [2]
[1, 2] >>= addOne == [2, 3]

[ ]    >>= addOne >>= addOne == [ ]
[1]    >>= addOne >>= addOne == [3]
[1, 2] >>= addOne >>= addOne == [3, 4]

[ ] >> [ ] == [ ]
[ ] >> [2] == [ ]
[1] >> [ ] == [ ]
[1] >> [2] == [2]

-- Maybe
Nothing >>= addOne == Nothing
Just 1  >>= addOne == Just 2

Nothing >>= addOne >>= addOne == Nothing
Just 1  >>= addOne >>= addOne == Just 3

Nothing >> Nothing == Nothing
Nothing >> Just 2  == Nothing
Just 1  >> Nothing == Nothing
Just 1  >> Just 2  == Just 2

-- Either
Left  1 >>= addOne == Left  1
Right 1 >>= addOne == Right 2

Left  1 >>= addOne >>= addOne == Left  1
Right 1 >>= addOne >>= addOne == Right 3

-- Either
Left  1 >> Left  2 == Left  1
Left  1 >> Right 2 == Left  1
Right 1 >> Left  2 == Left  2
Right 1 >> Right 2 == Right 2


-}
--IOMONAD 

-- We can use the bind operator to put through a sequence of actions.  For example 

foo8 = getLine >>= readFile >>= putStrLn

-- otherwise we can use the do notation for example. 

foo9 = do
    filename <- getLine
    contents <- readFile filename
    putStrLn contents




listOfTuples :: [(Int,Char)]  
listOfTuples = do  
    n <- [1,2]  
    ch <- ['a','b']  
    return (n,ch)  


 

 -- WRITER MONAD 



newhalf :: Int -> Writer String Int
newhalf x = do
        tell ("I just halved " ++ (show x) ++ "!")
        return (x `div` 2)

foo10 = runWriter $ newhalf 8 >>= newhalf


-- READER MONAD

-- the reader monad lets you pass a value to all your functions behind the scenes
-- return puts a value in a Reader

greeter :: Reader String String
greeter = do
    name <- ask
    return ("hello, " ++ name ++ "!")

foo11 = runReader greeter $ "Ben"


-- STATE MONAD 


greeter2 :: State String String
greeter2 = do
    name <- get
    put "Bob"
    return ("hello, " ++ name ++ "!")

foo12 = runState greeter2 $ "Ben"



-- TREES

data Tree a = Node a (Tree a) (Tree a) | Empty
    deriving Show -- to allow ghci to display trees


tree :: Tree Int
tree = Node 3 (Node 9 (Node 10 Empty Empty) (Node 5 Empty Empty)) (Node 7 Empty Empty) 


size :: Tree a -> Int
size Empty = 0
size (Node x l r) = 1 + size l + size r

height :: Tree a -> Int
height Empty        = 0
height (Node x l r) = 1 + (max (height l) (height r))


inorder :: (Ord a) => Tree a -> [a]
inorder Empty = []
inorder (Node x l r) = (inorder l) ++ [x] ++ (inorder r)

preorder :: (Ord a) => Tree a -> [a]
preorder Empty = []
preorder (Node x l r) = [x] ++ preorder l ++ preorder r

postorder :: (Ord a) => Tree a -> [a]
postorder Empty = []
postorder(Node x l r) = postorder l ++ postorder r ++ [x]

levelorder :: (Ord a) => Tree a -> [a]
levelorder t = step [t]
    where
      step [] = []
      step ts = concatMap elements ts ++ step (concatMap subtrees ts)
      elements Empty = []
      elements (Node x l r) = [x]
      subtrees Empty = []
      subtrees (Node x l r) = [l,r]

balanced :: Tree a -> Int
balanced Empty        = 0
balanced (Node x l r) = 1 + (max (height l) (height r))

-- Balanced Binary Tree 

data BST a = Empty1 | Node1 (BST a) a (BST a)

depth :: BST a -> Int
depth Empty1 = 0
depth (Node1 l _ r) = 1 + max (depth l) (depth r)

minDepth :: BST a -> Int
minDepth Empty1 = 0
minDepth (Node1 l _ r) = 1 + min (minDepth l) (minDepth r)

isBalanced :: BST a -> Bool
isBalanced tree = (depth tree - minDepth tree) <= 1 


-- 

sorted1 :: (Ord a) => [a] -> Bool
sorted1 [] = True
sorted1 [_] = True 
sorted1 (x1:x2:xs) = x1 <= x2 && sorted1 (x2:xs) 


-- this one is much better than sorted 1

sorted2 :: (Ord a) => [a] -> Bool
sorted2 [] = True 
sorted (x:xs) = sorted_lag x xs 

sorted_lag :: (Ord a) => a -> [a] -> Bool
sorted_lag _ [] = True 
sorted_lag x1 (x2:xs) = x1 <= x2 && sorted_lag x2 xs 




alternatingSums a = [sum x, sum y]
  where
    x = map (a !!) [0,2..length a -1]
    y = map (a !!) [1,3..length a -1]

alternatingSums1 [] = [0,0]
alternatingSums1 (a:[]) = [a,0]
alternatingSums1 (a:b:re) = zipWith (+) [a,b] (alternatingSums re)


arrayChange :: [Int] -> Int
arrayChange xs = sum(scanl1 (\acc x -> if x > acc then x else acc + 1 ) xs) - sum(xs)



scan :: [Int] -> Int -> Int
scan [x,y] n = max n (abs $ x - y)
scan (x:y:xs) n
  | a > n = scan (y:xs) a
  | otherwise = scan (y:xs) n
  where 
    a = abs $ (x - y)



isDot :: String -> Bool
isDot = isD . reverse
    where
    isD ('.':'/':_) = True
    isD ['.']       = True
    --isD ('/':_)     = False
    isD (_:cs)      = isD cs
    isD []          = False


