-- Logger monad
type Log = [String]
data Logger a = Logger a Log

--Instance of Logger in Eq
instance (Eq a) => Eq (Logger a) where
    (Logger x _) == (Logger y _) = x == y

--Instance of Logger in Show, must show a and list of log as \line acc -> "\n\t" ++ line ++ acc
instance (Show a) => Show (Logger a) where
    show (Logger d l) = show d ++ foldr (\line acc -> "\n\t" ++ line ++ acc) "" l

--Instance  of Logger in Functor define fmap 
instance Functor Logger where
    fmap f (Logger d l) = Logger (f d) l

--Instance of Logger in Applicative , define pure (i.e) the return and <*> to do so define the result (remember first logger has a function)
instance Applicative Logger where
    pure x = Logger x []
    (Logger f l) <*> (Logger x r) = Logger (f x) (l ++ r)

--Instance of Logger in Monad, define (Logger x l) >>= f to do so define the final result
instance Monad Logger where
    (Logger x l) >>= f =
        let Logger x' l' = f x
        in Logger x' (l ++ l')

--Define putLog, take a string and put into the Log of a Logger
putLog :: String -> Logger ()
putLog s = Logger () [s]

--Binary trees
data BTree a = BEmpty | BNode a (BTree a) (BTree a) deriving Eq

--bleaf take a number and return its tree
bleaf :: Int -> BTree Int
bleaf x = BNode x BEmpty BEmpty

--Instance of show for BTree
instance (Show a) => Show (BTree a) where
    show BEmpty = ""
    show(BNode x l r) = "<" ++ show l ++ show x ++ show r ++ ">" 

t1 = BNode 1 (bleaf 2) (BNode 3 (bleaf 4) (bleaf 5))
t2 = BNode 2 (BNode 2 (bleaf 3) (bleaf 2)) (bleaf 5)

--bleafM take a number and create its logger and its tree
--bleafM :: Int -> Logger (BTree Int)
bleafM x = do
    putLog $ "Created Leaf " ++ show x
    return $ bleaf x

--treeReplaceM take a tree and 2 number and return a logger, define empty ad tree case where you search for the first number and if found replace it with the second
treeReplaceM :: (Eq a, Show a) => BTree a -> a -> a -> Logger(BTree a)
treeReplaceM BEmpty _ _ = return BEmpty
treeReplaceM (BNode v l r) x y = do
    newl <- treeReplaceM l x y
    newr <- treeReplaceM r x y
    if v==x then do
        putLog $ "Node " ++ show v ++ " replaced with " ++ show y
        return $ BNode y newl newr
    else
        return $ BNode v newl newr

--buildTreeM take an int and return Logger(tree int), where the tree int has as child (int 'div' 2)
buildTreeM :: Int -> Logger(BTree Int)
buildTreeM 0 = bleafM 0
buildTreeM x = do
    putLog $ "Added node " ++ show x
    l <- buildTreeM (x `div` 2)
    r <- buildTreeM (x `div` 2)
    return $ BNode x l r



--LolStream, int is the number of elemenent of a periodic sequence
data LolStream a = LolStream Int [a]

--isPeriodic take a LolStream and return bool
isPeriodic :: LolStream a -> Bool
isPeriodic (LolStream n ls) = n > 0

--destream take a LolStream and return its list, if periodic take n element
destream :: LolStream a -> [a]
destream lol@(LolStream n ls)= if isPeriodic lol then take n ls else ls

--Instance of show  if periodic "LolStream[...]" else show the destream using pattern alias (@)
instance Show a => Show (LolStream a) where
    show l | not (isPeriodic l) = "LolStream[...]"
    show lol@(LolStream n l)= "LolStream" ++ show (destream lol)

--Instance of Eq 
instance Eq a => Eq (LolStream a) where
    x == y = destream x == destream y

--lolRepeat take a list and return a infinit list of the input
lolRepeat :: [a] -> [a]
lolRepeat ls = ls ++ lolRepeat ls

--lol2lolstream take a list of list and return a LolStream periodic of that list
lol2lolstream :: [[a]] -> LolStream a
lol2lolstream ls = LolStream (length ls') (lolRepeat ls') where
    ls' = concat ls

--Instance of Functor, fmap  
instance Functor LolStream where
    fmap f (LolStream n l) = LolStream n (fmap f l) 

--Instance of Foldable 
instance Foldable LolStream where
    foldr f z l = foldr f z (destream l)

--Instance of Applicative
instance Applicative LolStream where
    pure x = lol2lolstream [[x]]
    l1@(LolStream n fs) <*> l2@(LolStream m xs) = if (n > 0) && (m > 0) then 
        LolStream (n*m) (lolRepeat(destream l1 <*> destream l2))
        else LolStream (-1) (fs <*> xs)

--Instance Monad
instance Monad LolStream where
    ls >>= fs =  lol2lolstream [destream ls >>= \x -> destream (fs x)]


testM :: LolStream (Integer, Integer)
testM = do
    x <- lol2lolstream [[1..4]]
    y <- lol2lolstream [[2..5]]
    return (x,y)


main :: IO ()
main = do
    print testM







    
