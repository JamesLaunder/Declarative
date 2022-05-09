-- import Data.List
-- triangle :: [a] -> [Int]

-- triangle [] = []
-- triangle (x:xs)
   -- | x == (x:_) = [x]
   -- |


-- triangle [] = []
--triangle (x:xs) = concat (replicate n [x])
-- triangle xs = [elemIndices 2 xs]
--mainfun (x:xs) 
  --  | x = traignle xs n ++
   -- | otherwise mainfunc xs
-- repli xs n = concat (replicate n xs)


triangle :: [a] -> [a]
triangle = duplicate' 1
    where duplicate' _ [] = []
          duplicate' i (x:xs) = replik i
              where replik j | j > 0 = x : replik (j-1)
                          | otherwise = duplicate' (i+1) xs
   
duplicate' :: Int -> [a] -> [a]
duplicate' _ [] = []
duplicate' i (x:xs) = replik i
    where replik j | j > 0 = x : replik (j-1)
                | otherwise = duplicate' (i+1) xs
