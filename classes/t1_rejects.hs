{- Using Merge Sort -}
merge :: Ord a => [[a]] -> [[a]] -> [[a]]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | (length x) <= (length y) = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

msort :: Ord a => [[a]] -> [[a]]
msort [] = []
msort [[]] = [[]]
msort xs = merge (msort left) (msort right)
    where
        left = take (length xs `div` 2) xs
        right = drop (length xs `div` 2) xs

{- Insertion Sort -}

insertionSort :: Ord a => [[a]] -> [[a]]
insertionSort xs = insertionSortController xs (length xs)

insertionSortController :: Ord a => [[a]] -> Int -> [[a]]
insertionSortController xs 0 = xs
insertionSortController xs n = insertionSortController (isort xs) (n-1)

isort :: Ord a => [[a]] -> [[a]]
isort [] = []
isort [[a]] = [[a]]
isort (x:y:z)   | length x <= length y = x : isort (y:z)
                | otherwise = y : isort (x:z)
