-- Execute: ghci main.hs
-- Quit: ctrl + D

-- Multiplicate all numbers in array

grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m],y <- [0..n]]

square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n, x/=y]

-- Calculate array length
-- legth [1,2,3] = 3
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

-- Inverts array
-- reverse [1,2,3] = [3,2,1]
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- Joins 2 array into tuples
-- zip [1,2] [3,4] = [(1,3),(2,4)]
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : (zip' xs ys)

pdrop :: Int -> [a] -> [a]
pdrop 0 xs = xs
pdrop _ [] = []
pdrop n (_:xs) = pdrop (n-1) xs

-- unit :: [a] -> [a] -> [a]
-- unit [] unit ys = ys
-- unit xs unit [] = xs
-- unit (x:xs) unit ys = x:(xs unit ys)
