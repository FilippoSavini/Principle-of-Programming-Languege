-- Define a data-type called BTT which implements trees that can be binary or ternary, and where every
-- node contains a value, but the empty tree (Nil). Note: there must not be unary nodes, like leaves.
-- 1) Make BTT an instance of Functor and Foldable.
-- 2) Define a concatenation for BTT, with the following constraints:
-- • If one of the operands is a binary node, such node must become ternary, and the other operand
-- will become the added subtree (e.g. if the binary node is the left operand, the rightmost node of
-- the new ternary node will be the right operand).
-- • If both the operands are ternary nodes, the right operand must be appened on the right of the left
-- operand, by recursively calling concatenation.
-- 3) Make BTT an instance of Applicative.

data BTT a = BTEmpty | BTNode a (BTT a) (BTT a) | TTNode a (BTT a) (BTT a) (BTT a) deriving (Eq, Show)

instance Functor BTT where
    fmap _ BTEmpty = BTEmpty
    fmap f (BTNode a l r) = BTNode (f a) (fmap f l) (fmap f r)
    fmap f (TTNode a l c r) = TTNode (f a) (fmap f l) (fmap f c) (fmap f r)

instance Foldable BTT where
    foldr f z BTEmpty = z
    foldr f z (BTNode v l r) = foldr f (f v (foldr f z r)) l
    foldr f z (TTNode v l c r) = foldr f (f v (foldr f (foldr f z r) c)) l

(<++>) :: BTT a -> BTT a -> BTT a
x <++> BTEmpty = x
BTEmpty <++> x = x
(BTNode x l r) <++> y = TTNode x l r y
y <++> (BTNode x l r) = TTNode x y l r
(TTNode x l c r) <++> v@(TTNode x' l' c' r') = TTNode x l c (r <++> v)

ltconcat :: Foldable t => t (BTT a) -> BTT a
ltconcat = foldr (<++>) BTEmpty 
ltconcatmap :: (Foldable t, Functor t) => (a1 -> BTT a2) -> t a1 -> BTT a2
ltconcatmap f l = ltconcat $ fmap f l

instance Applicative BTT where
    pure x = BTNode x BTEmpty BTEmpty
    x <*> y = ltconcatmap (\f -> fmap f y) x 

t1 = BTNode 4 (TTNode 3 (BTNode 2 BTEmpty BTEmpty) BTEmpty BTEmpty) BTEmpty
t2 = BTNode 5 BTEmpty BTEmpty

main :: IO ()

main = do
    
    let t3 = BTNode 6 BTEmpty BTEmpty
    let t4 = BTNode 7 BTEmpty BTEmpty

    -- Create a list of BTT values
    let listOfBTTs = [t1, t2, t3, t4]

    -- Use ltconcat to concatenate the BTT values in the list
    let concatenatedBTT = ltconcat listOfBTTs

    -- Print the concatenated BTT
    print concatenatedBTT



