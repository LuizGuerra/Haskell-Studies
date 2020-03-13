




codigo01 :: Ord a => [a] -> Bool
codigo01 [] = True
codigo01 (x:y:tail) | x > y = False
              | otherwise = codigo01 (y:tail)
codigo01 (x:xs) = True


codigo02 :: Ord a => [a] -> Bool
codigo02 [] = True
codigo02 (x:y:tail) | x > y = False
              | otherwise = codigo02 (y:tail)

