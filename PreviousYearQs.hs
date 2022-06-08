-- This was an implementation by be n for 2021 exam
coins :: [Int] -> Int -> Bool
coins denoms n
| elem 0 denoms = True
| elem 1 denoms = True
| otherwise = coinsRec denoms n
coinsRec denoms n
| n == 1 = True
| elem n denoms = True
| otherwise = answer
where
withoutSingleCoin = filter (> 0) (map (\c -> n - c) denoms)
resultsOfAllVariants = map (\n1 -> coinsRec denoms n1) withoutSingleCoin
answer = elem True resultsOfAllVariants
