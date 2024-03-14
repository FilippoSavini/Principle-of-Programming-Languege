-- Define a partitioned list data structure, called Part, storing three elements:
-- 1. a pivot value,
-- 2. a list of elements that are all less than or equal to the pivot, and
-- 3. a list of all the other elements.
-- Implement the following utility functions, writing their types:
-- • checkpart, which takes a Part and returns true if it is valid, false otherwise;
-- • part2list, which takes a Part and returns a list of all the elements in it;
-- • list2part, which takes a pivot value and a list, and returns a Part;
-- Make Part an instance of Foldable and Functor, if possible. If not, explain why.

data Part a = Part a [a] [a]

checkpart :: (Ord a) => Part a -> Bool
checkpart (Part a x y) = null (filter(\v -> a >= v) x) && null (filter(\v -> a < v) y)

part2list :: Part a -> [a]
part2list (Part a x y) = x ++ [a] ++ y 

list2part :: (Ord a) => a -> [a] -> Part a
list2part a x = Part a (filter(\v -> a >= v) x) (filter(\v -> a < v) x)

-- instance Functor Part where
--     fmap f (Part a x y) = list2part (f a) $ fmap f (part2list (Part a x y))
-- is not possible to make it an instance of Functor since becouse f (a -> b) but list2part require Ord and this constrain can't be specified in the fmap

instance Foldable Part where
    foldr f acc x= foldr f acc (part2list x)
