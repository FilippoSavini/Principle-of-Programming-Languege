-- Consider a Tvtl (two-values/two-lists) data structure, which can store either two values of a given type, or
-- two lists of the same type.
-- Define the Tvtl data structure, and make it an instance of Functor, Foldable, and Applicative.

data Tvtl a = Tv a a | Tl [a] [a] deriving Show

instance Foldable Tvtl where
    foldr f z (Tv x y) = f x (f y z)
    foldr f z (Tl x y) = foldr f (foldr f z y) x

instance Functor Tvtl where
    fmap f (Tv x y) = Tv (f x) (f y)
    fmap f (Tl x y) = Tl (fmap f x) (fmap f y)


(+++) :: Tvtl a -> Tvtl a -> Tvtl a
(Tv x y) +++ (Tv w z) = Tl [x, w] [y, z]
(Tv x y) +++ (Tl w z) = Tl (x : w) (y : z)
(Tl x y) +++ (Tv w z) = Tl (x ++ [w]) (y ++ [z])
(Tl x y) +++ (Tl w z) = Tl (x ++ w) (y ++ z)


tvtlconcat :: Tvtl (Tvtl a) -> Tvtl a
tvtlconcat t = foldr (+++) (Tl [][]) t


tvtlconcatmap :: (a1 -> Tvtl a2) -> Tvtl a1 -> Tvtl a2
tvtlconcatmap f t = tvtlconcat $ fmap f t

instance Applicative Tvtl where
    pure x = Tl [x] []
    x <*> y = tvtlconcatmap (\f -> fmap f y) x


tvtlToString (Tv x y) = "Tv " ++ show x ++ " " ++ show y
tvtlToString (Tl xs ys) = "Tl " ++ show xs ++ " " ++ show ys

main :: IO ()
main = do
    let tv1 :: Tvtl (Int -> Int)
        tv1 = Tl [(+1), (*2)] []

    let tv2 :: Tvtl Int
        tv2 = Tv 5 3

    -- Apply <*> operator to get the result
    let result :: Tvtl Int
        result = tv1 <*> tv2

    -- Convert the result to a string and print it
    putStrLn $ tvtlToString result

