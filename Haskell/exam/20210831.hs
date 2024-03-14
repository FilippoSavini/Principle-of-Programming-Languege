-- Consider a Slist data structure for lists that store their length. Define the Slist data structure, and make it
-- an instance of Foldable, Functor, Applicative and Monad.

data Slist a = Slist Int [a] deriving (Eq, Show)

makeSlist :: [a] -> Slist a
makeSlist v = Slist (length v) v

instance Foldable Slist where
    foldr f z (Slist n xs) = foldr f z xs

instance Functor Slist where
    fmap f (Slist n xs) = Slist n (fmap f xs)

instance Applicative Slist where
    pure x = Slist 1 (pure x)
    (Slist n fs)<*>(Slist y xs) = Slist (n*y) (fs<*>xs)

instance Monad Slist where
    (Slist n xs) >>= f = makeSlist (xs >>= (\x -> let Slist n ys = f x in ys))

-- A function that doubles each element
double :: Int -> Slist Int
double x = makeSlist [x * 2]

main :: IO ()
main = do
    -- Slist containing [1, 2, 3]
    let slist1 :: Slist Int
        slist1 = makeSlist [1, 2, 3]

    -- Using the correct implementation
    let result :: Slist Int
        result = slist1 >>= double

    putStrLn $ "Result: " ++ show result
