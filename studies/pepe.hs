contain x xs = or [ x' == x | x' <- xs]

difference :: Eq a => [a] -> [a] -> [a]
difference [] _ = []
difference a [] = a
difference (a:as) bs  | contain a bs = difference as bs
                      | otherwise = a : difference as bs
--

