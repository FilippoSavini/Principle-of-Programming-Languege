--Length of a list of Integer
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
mylength :: [Int] -> Int
mylength [] = 0
mylength (_ : xs) = 1 + mylength xs

--Lenght of a list of generic element
mylength2 :: [a] -> Int
mylength2 [] = 0
mylength2 (_ : xs) = 1 + mylength2 xs

--Range
myrange :: Int -> Int -> [Int]
myrange a b
    | a > b = error "Low>High"
    | a == b = [a]
    | a < b = a : myrange (a+1) b

--Map
mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (x : xs) = f x :mymap f xs

--List comprehension
rightTriangoles :: [(Int, Int, Int)]
rightTriangoles = [(a, b, c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2+b^2 == c^2, a + b + c == 24]

--Takewhile, take value untill the condition is true
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile f (x : xs)
    | f x = x : myTakeWhile f xs 
    | otherwise = []

--Pack 
packHelper :: Eq a => [a] -> [[a]] -> [a] -> [[a]]
packHelper [] acc sub = sub : acc
packHelper (x : xs) acc [] = packHelper xs acc [x]
packHelper (x : xs) acc (y : ys)
    | x == y = packHelper xs  acc (x : sub)
    | otherwise = packHelper xs (sub : acc) [x]
    where sub = y : ys

--packHelper ["m", "i", "s", "s", "i", "s", s", "i", "p", "p", "i"] [] []
--packHelper "ississippi" [] ['m']
--packHelper "ssissippi" [m] ['i']
--packHelper "sissippi" [i, m] ['s']
--packHelper "issippi" [i, m] ['s', 's']
--packHelper "ssippi" [ss, i, m] ['i']
--packHelper "sippi" [i, ss, i, m] ['s']
--packHelper "ippi" [i, ss, i, m] ['s', 's']
--packHelper "ppi" [ss, i, ss, i, m] ['i']
--packHelper "pi" [i, ss, i, ss, i, m] ['p']
--packHelper "i" [i, ss, i, ss, i, m] ['p', 'p']
--packHelper "" [pp, i, ss, i, ss, i, m] ['i'] > ret ["i", "pp", "i", "ss", "i", "ss", "i", "m"]

-- Final result, reversing the accumulated list: (with pack)
--["m","i","ss","i","ss","i","pp","i"]

pack :: Eq a => [a] -> [[a]]
pack input = reverse (packHelper input [] [])

--Encode return a list of couple (elem, num_elem) given a string
encode :: Eq a => [a] -> [(a, Int)]
encode input = zip (map head packed) (map length packed)
    where packed = pack input

--Binary trees
-- let empty = EmptyTree
-- let singleNode = Node 10 EmptyTree EmptyTree
-- let twoNodes = Node 5 empty singleNode
-- ...
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- Implementing a type class
-- 
-- Definition from the Prelude:
-- class Eq a where
--     (==) :: a -> a -> Bool
--     (/=) :: a -> a -> Bool
--     x == y = not (x /= y)
--     x /= y = not (x == y)
--
-- This means that a) we have to implement both "equals" and "not equals"
-- and b) since "x is equal to y if x is not equal to y" and viceversa,
-- we can just define "equals" or "not equals" and Haskell will infer the
-- other one.

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Yellow == Yellow = True
    Green == Green = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red"
    show Yellow = "Yellow"
    show Green = "Green"


main :: IO ()
main = do 
    print(myrange 4 6)



