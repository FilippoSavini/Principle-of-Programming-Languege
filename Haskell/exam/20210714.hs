-- 1) Define a "generalized" zip function which takes a finite list of possibly infinite lists, and returns a
-- possibly infinite list containing a list of all the first elements, followed by a list of all the second elements,
-- and so on.
-- E.g. gzip [[1,2,3],[4,5,6],[7,8,9,10]] ==> [[1,4,7],[2,5,8],[3,6,9]]
-- 2) Given an input like in 1), define a function which returns the possibly infinite list of the sum of the two
-- greatest elements in the same positions of the lists.
-- E.g. sum_two_greatest [[1,8,3],[4,5,6],[7,8,9],[10,2,3]] ==> [17,16,15]
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

gzip :: [[a]] -> [[a]]
gzip x = if not (any null x)
    then map head x : gzip (map tail x)
    else []


storetwogreatest :: Ord a => a -> (a, a) -> (a, a)
storetwogreatest v (x,y) | v > x = (v,y)
storetwogreatest v (x,y) | x >= v && v > y = (x,v)
storetwogreatest v (x,y) = (x,y)


findtwogreatest :: Ord a => [a] -> (a, a)
findtwogreatest (x : y : xs) = foldr storetwogreatest (if x > y then (x, y) else (y, x)) xs

sumtwogreatest :: (Num a, Ord a) => [[a]] -> [a]
sumtwogreatest xs = [let (x, y) = findtwogreatest v in x+y | v <- gzip xs]
