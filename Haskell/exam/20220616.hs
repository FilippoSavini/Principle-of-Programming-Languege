-- Consider the "fancy pair" data type (called Fpair), which encodes a pair of the same type a, and may
-- optionally have another component of some "showable" type b, e.g. the character '$'.
-- Define Fpair, parametric with respect to both a and b.
-- 1) Make Fpair an instance of Show, where the implementation of show of a fancy pair e.g. encoding
-- (x, y, '$') must return the string "[x$y]", where x is the string representation of x and y of y. If the third
-- component is not available, the standard representation is "[x, y]".
-- 2) Make Fpair an instance of Eq — of course the component of type b does not influence the actual
-- value, being only part of the representation, so pairs with different representations could be equal.
-- 3) Make Fpair an instance of Functor, Applicative and Foldable
{-# LANGUAGE InstanceSigs #-}

data Fpair s a = Pair a a | Fpair a a s

instance (Show a, Show s) => Show(Fpair s a) where
    show (Pair a b) = "[" ++ show a ++ "," ++ show b ++ "]"
    show (Fpair a b s) = "[" ++ show a ++ show s ++ show b ++ "]"

instance (Eq a) => Eq (Fpair s a) where
    Fpair x y z == Fpair a b c = x == a && y == b
    Fpair x y z == Pair a b = x == a && y == b
    Pair x y == Fpair a b c = x == a && y == b
    Pair x y == Pair a b = x == a && y == b

instance Functor (Fpair s) where
    fmap :: (a -> b) -> Fpair s a -> Fpair s b
    fmap f (Pair a b) = Pair (f a) (f b)
    fmap f (Fpair a b c) = Fpair (f a) (f b) c

instance Applicative (Fpair s) where
    pure x = Pair x x
    (Pair f g)<*>(Pair x y) = Pair (f x) (g y)
    (Fpair f g s)<*>(Pair x y) = Fpair (f x) (g y) s
    (Pair f g)<*>(Fpair x y s) = Fpair (f x) (g y) s
    (Fpair f g _)<*>(Fpair x y z) = Fpair (f x) (g y) z

instance Foldable (Fpair s) where
    foldr f i (Pair a b) = f a (f b i) 
    foldr f i (Fpair a b _) = f a (f b i) 







