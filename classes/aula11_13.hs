-- module FlexibleContextss


-- mapFunction :: (a -> b) -> [a] -> [b]
-- mapFunction f xs = [ f x | x <- xs ]

-- filterFunction :: (a -> Bool) -> [a] -> [a]
-- filterFunction f xs = [ x | x <- xs, f x ]

-- doumap :: (a -> b) -> (a -> b) -> [a] -> [b]
-- doumap _ _ [] = []
-- doumap f p xs = [ f x | x <- xs ] ++ [ p x | x <- xs ]

-- anmap :: (a -> b) -> [a] -> [b]
-- anmap f xs = [ f x | x <- xs ]

-- anotherone :: (a -> b) -> [a] -> [b]
-- anotherone f [] = []
-- anotherone f (x:xs) = f x : anotherone xs

-- atry a [] = []
-- atry a (b:bs) = a b : atry a bs

-- nammm :: (a -> b) -> [a] -> [b]

nammm f [] = []
nammm f (x:xs) = f x : nammm f xs



                                    {- AULA 13 -}


type Pos = (Int, Int)

data Bool = True|False

fun :: Pos -> Int
fun (a,b) = a+b

data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect l h) = l * h

data Natt = Zero | Succ Natt


