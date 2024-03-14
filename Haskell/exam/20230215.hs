import Distribution.Simple.Utils (xargs)
-- We want to define a data structure for binary trees, called BBtree, where in each node are stored two values of the
-- same type. Write the following:
-- 1. The BBtree data definition.
-- 2. A function bb2list which takes a BBtree and returns a list with the contents of the tree.
-- 3. Make BBtree an instance of Functor and Foldable.
-- 4. Make BBtree an instance of Applicative, using a “zip-like” approach, i.e. every function in the first
-- argument of <*> will be applied only once to the corresponding element in the second argument of <*>.
-- 5. Define a function bbmax, together with its signature, which returns the maximum element stored in the
-- BBtree, if present, or Nothing if the data structure is empty.

data BBtree a = BBtree a a (BBtree a) (BBtree a) | BNil

bb2list :: BBtree a -> [a]
bb2list BNil = []
bb2list (BBtree x y lt rt) = [x] ++  [y] ++  bb2list lt ++ bb2list rt

instance Functor BBtree where
    fmap f BNil = BNil
    fmap f (BBtree x y lt rt) = BBtree (f x) (f y) (fmap f lt) (fmap f rt)

instance Foldable BBtree where
    foldr f acc BNil = acc
    foldr f acc (BBtree x y lt rt) = f x (f y (foldr f (foldr f acc rt) lt))

instance Applicative BBtree where
    pure x = BBtree x x BNil BNil
    fs <*> BNil = BNil
    BNil <*> xs = BNil
    (BBtree fx fy flt frt) <*> (BBtree x y lt rt) = BBtree (fx x) (fy y) (flt <*> lt) (frt <*> rt)

bbmax :: (Ord a) =>  BBtree a -> Maybe a
bbmax BNil = Nothing
bbmax t@(BBtree x y lt rt) = Just $ foldr max x t


