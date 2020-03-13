
twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [a] = True
sorted (x:y:tail)   | x > y = False
                    | otherwise = sorted (y:tail)

remove :: Int -> [Int] -> [Int]
remove _ [] = []
remove n xs | n == x = xs
                | otherwise = x : remove n xs
--




