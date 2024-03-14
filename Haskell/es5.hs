
--Define reverse and filter with fold
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) []

myFilter  :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x:acc else acc) []

-- Binary trees
-- let nums = [8,6,4,1,7,3,5]
-- let t = foldr treeInsert EmptyTree nums
-- (foldr because treeInsert takes the current tree as second parameter)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Eq, Show, Read)

--singleton create a node given a value
singleton :: a -> Tree a
singleton a = Node a EmptyTree EmptyTree

--treeInsert take the current tree as the second parameter and add to it the first parameter
treeInsert :: (Ord a) => a -> Tree a -> Tree a --(Ord a) => is required since element of type a must be instances of Ord in other words <, >, ==, must be defined for a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x==a = Node x left right
    | x>a = Node a left (treeInsert x right)
    | x<a = Node a (treeInsert x left) right

--treeElem take as input an elem and a tree and return true if elem is in the tree
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x==a = True
    | x<a = treeElem x left
    | x>a = treeElem x right

--treeSum return the sum of the value of the node inside the tree
treeSum :: Num a => Tree a -> a
treeSum EmptyTree = 0
treeSum (Node a left right) = a + treeSum left + treeSum right

--treeValue take a tree and return a list of is value
treeValue :: Tree a -> [a]
treeValue EmptyTree = []
treeValue (Node a left right) = treeValue left ++ [a] ++ treeValue right

--treeMap map a f in a tree
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ EmptyTree = EmptyTree
treeMap f (Node a left right) = Node (f a) (treeMap f left) (treeMap f right)

--treeFold
treeFold :: (a -> b -> a) -> a -> Tree b -> a
treeFold f x EmptyTree = x
treeFold f x (Node y left right) = treeFold f (f (treeFold f x left) y) right

-- You can think the typeclass Functor as simply a typeclass used to be able to apply
-- a function to each element of your structure. Basically, any data structure that
-- works as a container for some data, can be a Functor
-- (cfr. the books for the theory that goes behind the concept of Functor)
instance Functor Tree where
        fmap f EmptyTree = EmptyTree
        fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)



main :: IO()
main = do
    let a = treeInsert 2 (singleton 3)
    print(treeSum a)