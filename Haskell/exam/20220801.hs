-- We want to implement a binary tree where in each node is stored data, together with the number of nodes
-- contained in the subtree of which the current node is root.
-- 1. Define the data structure.
-- 2. Make it an instance of Functor, Foldable, and Applicative.

data BTree a = Node a Int (BTree a) (BTree a) | TNil
nnode :: BTree a -> Int
nnode TNil = 0
nnode (Node a n lf rt) = n

instance Functor BTree where
    fmap f TNil = TNil
    fmap f (Node a n lf rt) = Node (f a) n (fmap f lf) (fmap f rt)

instance Foldable BTree where
    foldr f acc TNil = acc
    foldr f acc (Node a n lf rt) = f a (foldr f (foldr f acc lf) rt)

(+++) :: BTree a -> BTree a -> BTree a
x +++ TNil = x
TNil +++ x = x
(Node a n lf rt) +++ t = Node a (n + (nnode t)) lf (rt +++ t)

tconcat :: Foldable t => t (BTree a) -> BTree a
tconcat = foldr (+++) TNil
tconcmap :: (Foldable t, Functor t) => (a1 -> BTree a2) -> t a1 -> BTree a2
tconcmap f t = tconcat $ fmap f t

instance Applicative BTree where
    pure x = Node x 0 TNil TNil
    fs <*> xs = tconcmap (\f -> fmap f xs) fs

