import Data.List
import Data.Char
solution n = map (\x -> (read [x] :: Int)) (show n)


pal inputString = filter odd $ map length $ group $ sort inputString

sol inputArray k = filter(/=inputArray !! (k-1)) inputArray

lineEncoding s = concat $ map encode $ group s
    where encode [c] = [c]
          encode s = (show $ length s) ++ [head s]
    -- zip (nub s) $ map length $ group $ sort s

delDigit n = (filter (/= (head $ sort $ show $ n)) $ show $ n)


-- longestWord text = splitWhen (not.isAlpha) text
    -- filter ((\x -> length x >= n)) x where n = maximum $ map length x
    -- map length $ text
-- longestWord "Ready, steady, go!"
-- sumNum x = sum . map read . filter (isDigit . head) . groupBy areDigits
-- areDigits x y = isDigit x == isDigit y

-- digitsproduct x = digitToInt ((show $ x) !! 1)

myButLast x =  head . tail . reverse

newList xs = scanl1 (\acc x -> if x > acc then x else acc) 


-- newList2 :: [Int] -> [Int]
-- newList2 xs =  getLength $ group $ scanl1 (\acc x -> if x > acc then x else acc) xs
-- -- lengthFilter $ 

-- getLength :: [[Int]] -> [Int]
-- getLength [x] = [length x]
-- getLength (x:xs) = [length x] ++ getLength xs


-- lengthFilter :: [Int] -> Bool
-- lengthFilter xs 
--      | length [x | x <- xs, (x > 2)] > 1 = False
--      | length [x | x <- xs, (x > 3)] >= 1 =False 
--      | otherwise = True

myStringScan :: [String] -> [String]
myStringScan xs =  scanl1 (\acc x -> if length x >= length acc then x else acc) xs

monad = [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)  