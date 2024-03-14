-- Consider a data structure Gtree for general trees, i.e. trees containg some data in each node, and a
-- variable number of children.
-- 1. Define the Gtree data structure.
-- 2. Define gtree2list, i.e. a function which translates a Gtree to a list.
-- 3. Make Gtree an instance of Functor, Foldable, and Applicative

data Gtree a = Node a [Gtree a] | GEmpty deriving Show

gtree2list :: Gtree a -> [a]
gtree2list GEmpty = []
gtree2list (Node x y) = x : concatMap gtree2list y

instance Functor Gtree where
    fmap _ GEmpty = GEmpty
    fmap f (Node a y) = Node (f a) (fmap (fmap f) y)

instance Foldable Gtree where
    foldr f acc x = foldr f acc $ gtree2list x

instance Applicative Gtree where
    pure x = Node x []
    fs <*> y = gtConcatMap (\f -> fmap f y) fs


GEmpty +++ GEmpty = GEmpty
GEmpty +++ Node x xs = Node x xs 
Node x xs +++ GEmpty = Node x xs 
Node x xs +++ Node y ys = Node y ((Node x []:xs) ++ ys)

gtConcat = foldr (+++) GEmpty 
gtConcatMap f x = gtConcat $ fmap f x

main :: IO()
main = do
    let tree = Node 2 [Node 4 [Node 8 []], Node 1 [Node 3 []]] 
    let treeOps = Node (+ 2) [Node (* 2) []]
    print (treeOps <*> tree)