
halve :: [a] -> ([a], [a])
halve [] = ([],[])
halve xs    | odd (length xs) = ([],[])
            | otherwise = ((take ((length xs) `div` 2) xs), (drop ((length xs) `div` 2) xs))

halve' xs = (take n xs, drop n xs)
    where n = length xs `div` 2

halve'' xs = splitAt (length xs `div` 2) xs

{- Segundo exercício -}

at2a :: [a] -> a
at2a [] = undefined
at2a (_:[]) = undefined
at2a (_:_:[]) = undefined
at2a (_:_:e) = head e

at2b :: [a] -> a
at2b xs  | length xs < 3 = undefined
        | otherwise = xs !! 2

third1 xs = head ( tail ( tail xs ) )
third2 xs = xs !! 2
third3 (_:_:x:_) = x

{- Segundo exercício -}

luhnDouble :: Int -> Int
luhnDouble val = if (val*2 < 9) then (val*2) else (val*2-9)

divisible :: Int -> Int -> Bool
divisible a b = if (a `mod` b == 0) then True else False

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = divisible (sum [luhnDouble a, b, luhnDouble c, d]) 10

    {-                         -------                         -}
    {-                         Aula 07                         -}
    {-                         -------                         -}

{- Compreensão de Listas -}
rdvlw = [ (x,y) | x <- [0..2], y <- [2..5]]

concat' :: [[a]] -> [a]
concat' xss = [ x | xs <- xss, x <- xs ]

{- Guardas -}
rdvlw2 = [ x | x <- [0..10], even x]

factors n = [ x | x <- [1..n], n `mod` x == 0 ]

prime n = factors n == [1,n]

primes n = [ x | x <- [1..n], prime x]

{- Função Zip -}

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [ x <= y | (x,y) <- pairs xs ]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [ i | (x', i) <- zip xs [0..], x == x']

{- Compreensão de Strings -}

count :: Char -> String -> Int
count x xs = length [ x' | x' <- xs, x == x' ]

    {-                         -------                         -}
    {-                         Aula 08                         -}
    {-                         -------                         -}

-- Questão 01
exerc1 = [ x*x | x <- [0..100] ]

-- Questão 02
exerc2 m n = [ (x,y) | x <- [0..m], y <- [0..n] ]

-- Questão 03
exerc3 m n = [ (x,y) | (x,y) <- exerc2 m n, x /= y ]

-- Questão 04
exerc4 :: Int -> a -> [a]
exerc4 0 _ = []
exerc4 ammount value = [value] ++ exerc4 (ammount-1) value

exerc4' :: Int -> a -> [a]
exerc4' ammount value   | ammount == 0 = []
                        | otherwise = [value] ++ exerc4' (ammount-1) value
                    
exerc4'' n v = [ v | _ <- [1..n] ]

-- poss :: Eq a => a -> [a] -> [Int]
-- poss k xs =  find k (zip xs [0..])

-- Questão 05
exerc5 :: Eq a => a -> [(a,b)] -> [b]
exerc5 k t = [ v | (k',v) <- t, k == k' ]

-- exerc5 1 [(0, "Text X"), (1, "Answer is B"), (1, "Text Find"), (0, " Blabla"), (0, "AB"), (1, "Kappa answer ma man"), (1, "Answer is B")]

-- entender como fazer o exerc06
-- ex 06
exerc6 = concat [ [(x,y) | y <- [3,4]] | x <- [1,2] ] 

-- Questão 07
pyth :: Int -> Int -> Int -> Bool
pyth x y z = (x*x)+(y*y) == (z*z)

val1 n = [ (x,y) | x <- [1..n], y <- [1..n] ]
val2 n = [ (x,y,z) | (x,y) <- val1 n, z <- [1..n] ]

exerc7 :: Int -> [(Int, Int, Int)]
exerc7 n = [ (x,y,z) | (x,y,z) <- val2 n, pyth x y z ]

teste n = [(x,y,z)| x<-[1..n], y<-[1..n], z<-[1..n], (x^2+y^2 == z^2) ]

-- Questão 08
divs n = [ x | x <- [1..n-1], n `mod` x == 0 ]
isPerfect n = sum (divs n) == n
perfects n = [ x | x <- [1..n], isPerfect x ]

-- Questão 09
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum ( [ x * y | (x,y) <- zip xs ys ] )


    {-                         -------                         -}
    {-                         Aula 09                         -}
    {-                         -------                         -}

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

allTrue :: [Bool] -> Bool
allTrue [] = True
allTrue (x:xs) = if x then allTrue xs else False

concatLists :: [[a]] -> [a]
concatLists [] = []
concatLists (x:xs) = x ++ concatLists xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' a b = b : replicate' (a-1) b

get :: Int -> [a] -> a
get 0 (x:_) = x
get v (_:xs) = get (v-1) xs

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)  | a == x = True
                | otherwise = elem' a xs


merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [a] = [a]
msort xs = merge (msort left) (msort right)
    where
        left = take (length xs `div` 2) xs
        right = drop (length xs `div` 2) xs



myAll :: (a -> Bool) -> [a] -> Bool
myAll f xs = and [ f x | x <- xs]

allR :: (a -> Bool) -> [a] -> Bool
allR _ [] = True
allR f (x:xs) = f x && allR f xs
