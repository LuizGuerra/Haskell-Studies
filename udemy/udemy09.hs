example01 = 
    let x = 8
        y = 5
    in print (x * x + y)

example02 = let {a = 22; b = 20} in print (a * b - b)

example03 = let {
a = 300;
b = 230;
c = 20;
d = 22;
} in print (a * b + (c * d - c))

nice = putStrLn "Noice"



-- f= do
--     a
--     b
--     c

-- g = do a
--        b
--        c

-- a :: IO ()
-- main = undefined

f x y = a + b
    where 
        a = x
        b = y

g x = case x of p0 -> a
                p1 -> b

a = undefined; b = undefined; c = undefined

-- formatList s e sep xs = s ++ (intercalate sep (map show xs)) ++ e

-- formatList "( )" ", " "[1, 2, 3, 4]" 

-- main = do
--     putStrLn "Say a number!"
--     x <- readLn
--     if x == 4
--         then putStrLn "You are right!"
--         else putStrLn "You are wong!"

