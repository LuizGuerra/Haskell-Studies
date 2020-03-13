-- QuickSort
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++  [x] ++ qsort larger
    where
        smaller = [a|a <- xs, a <= x]
        larger =  [b|b <- xs, b >  x]

-- Array Products
prod :: [Int] -> Int
prod [] = 1
prod (x:xs) = x * prod xs

-- Backwords QuickSort
qsort' :: Ord a => [a] -> [a]
qsort' [] = []
qsort' (x:xs) = qsort' larger ++  [x] ++ qsort' smaller
    where               
        smaller = [a|a <- xs, a<= x]
        larger  = [b|b <- xs, b > x]

-- ??
-- let Nnn = a `div` length xs
--     where
--         a = 10
--         xs = [1,2,3,4,5]

add :: Int -> (Int -> Int)
add x y = x + y

produtorio :: [Int] -> Int
produtorio [] = 1
produtorio (x:xs) = x * produtorio xs

isPar :: Int -> Int
isPar value = if (value `mod` 2 == 0) then 1 else 0

ternary :: Bool -> a -> a -> a
ternary a b c = if a then b else c

{-                   AULA 05                   -}


-- Absolute value of a value
-- {abso 10} = 10 // {abso (-10) = -10} 
abso :: Int -> Int
abso n = if n>=0 then n else -n

{- EQUAÇÕES RESTRITAS ou GUARDED EQUATIONS -}

absl :: Int -> Int
absl n  | n >= 0 = n
        | otherwise = -n

sick :: Int -> Int
sick n  | n < 0  = -1
        | n == 0 = 0 
        | otherwise = 1
--

addk x y = x + y

addj = \ x -> ( \ y -> x + y)

odds1 n = map f [0 .. n-1]
    where
        f x = x*2 + 1
        
odds2 n = map (\ x -> x*2 + 1) [0 .. n-1]

-- Returns the tail after some given index
-- { safeTail 2 [1,2,3,4,5] } = [3,4,5]
safeTail :: Int -> [a] -> [a]
safeTail 0 x = x
safeTail n _ | n < 0 = 
    error "Impossible to perform action with negative value"
safeTail _ [] = []
safeTail n (x:xs) = safeTail (n-1) xs


{-  multiplicação normal -}
mult1 :: Int -> Int -> Int -> Int
mult1 x y z = x * y * z

{- multiplicação lambda -}
-- mult2 :: Int -> Int -> Int -> Int
mult2 = \ x -> (\ y -> (\ z -> x * y * z) ) 

-- aula 12:
-- ex1 ever (>5) [1..10]

