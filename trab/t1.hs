{- Students: Luiz Pedro Franciscatto Guerra and Marcius Vargas -}

-- 1) Construa um programa que ordena em ordem ascendente 
-- uma lista de listas a partir do tamanho das sublistas.

{- Quick sort implementation to arrange an array of arrays -}
quickSort :: Ord a => [[a]] -> [[a]]
quickSort [] = []
quickSort (x:xs) = quickSort smaller ++ [x] ++ quickSort bigger
    where
        smaller = [ x' | x' <- xs, length x' <= length x ]
        bigger = [ x' | x' <- xs, length x' > length x ]


{- Test cases I've used: -}
{- [[1,2,3,4,5],[4,5,6,7],[7,8,9,10,11]] -}
{- [[0,1],[0,1,2],[0,1,2,3],[0,1,2,3],[0,1,2,3,4,5,6,7],[0,1,2,3],[0,1,2,3,4,5],[0,1,2,3,4,5,6],[0,1,2,3,4,5,6,7,8,9],[0,1,2]] -}
{- [[1,2,3,4,5],[4,5,6,7],[7,8,9]] -}

-- 2) Defina a função myMap :: (a -> b) -> (a -> b) -> [a] -> [b]
-- que aplica de forma alternada duas funções passadas como argumentos
-- aos elementos de uma lista. 

{- Map function that apply 2 functions alternatively -}
-- myMap :: (a -> b) -> (a -> b) -> [a] -> [b]
-- myMap _ _  [] = []
-- myMap f _ [a] = [f a]
-- myMap f p (x:y:t) = f x : p y : myMap f p t 

myMap :: (a -> b) -> (a -> b) -> [a] -> [b]
myMap _ _ [] = []
myMap f p (x:xs) = f x : myMap p f xs

-- 3) A partir da função myMap, defina um função luhn :: [Int] -> Bool
-- que implemente o algoritmo de Luhn para a validações de números de 
-- cartão de crédito para códigos de cartão de qualquer tamanho

{- Calculates the luhn double -}
luhnDouble :: Int -> Int
luhnDouble val = if (val*2 < 9) then (val*2) else (val*2-9)

{- Verify if all the array is zeros -}
allZero :: [Int] -> Bool
allZero [] = False
allZero xs = and [ x == 0 | x <- xs ]

{- Search for negative digits or double digits -}
wrongDigit :: [Int] -> Bool
wrongDigit [] = False
wrongDigit (x:xs) = if ( (x < 0) || (x > 9) ) then True else wrongDigit xs

{- Luhn Algorithm -}
luhn :: [Int] -> Bool
luhn xs | allZero xs = False
        | wrongDigit xs = False
        | otherwise = sum ( myMap luhnDouble (+0) xs ) `mod` 10 == 0


{- Test cases I've used -}
{- luhn [1,7,8,4] = True -}
{- luhn [4,7,8,3] = False -}
{- luhn [0,10,0] = False -}
{- luhn [0,0,0,0] = False -}

-- 4) Construa um programa em Haskell capaz de converter um número
-- octal (na forma forma de string) em um número decimal. Trate uma
-- entrada inválida com 0 octal. Não use funções prontas de conversão,
-- construa a sua própria versão usando suas próprias funções ou as
-- funções disponíveis no prelude.hs.

{- Octal switch conversion from char to number -}
switch :: Char -> Int
switch n    | n == '0' = 0
            | n == '1' = 1
            | n == '2' = 2
            | n == '3' = 3
            | n == '4' = 4
            | n == '5' = 5
            | n == '6' = 6
            | n == '7' = 7
            | otherwise = error "Impossible value given"

{- Converts octal to decimal -}
octalToDecimal :: String -> Int
octalToDecimal "" = 0
octalToDecimal (x:xs) = switch x * 8^(length xs) + octalToDecimal xs


myfunc n = if n == '0' then 0
    else if n == '1'then 1
    else ...
