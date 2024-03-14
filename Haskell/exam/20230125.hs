-- We want to define a data structure for the tape of a Turing machine: Tape is a parametric data structure with
-- respect to the tape content, and must be made of three components:
-- 1. the portion of the tape that is on the left of the head;
-- 2. the symbol on which the head is positioned;
-- 3. the portion of the tape that is on the right of the head.
-- Also, consider that the machine has a concept of "blank" symbols, so you need to add another component in the
-- data definition to store the symbol used to represent the blank in the parameter type.
-- 1. Define Tape.
-- 2. Make Tape an instance of Show and Eq, considering that two tapes contain the same values if their stored
-- values are the same and in the same order, regardless of the position of their heads.
-- 3. Define the two functions left and right, to move the position of the head on the left and on the right.
-- 4. Make Tape an instance of Functor and Applicative.
-- We want to define a data structure for the tape of a Turing machine: Tape is a parametric data structure with
-- respect to the tape content, and must be made of three components:
-- 1. the portion of the tape that is on the left of the head;
-- 2. the symbol on which the head is positioned;
-- 3. the portion of the tape that is on the right of the head.
-- Also, consider that the machine has a concept of "blank" symbols, so you need to add another component in the
-- data definition to store the symbol used to represent the blank in the parameter type.
-- 1. Define Tape.
-- 2. Make Tape an instance of Show and Eq, considering that two tapes contain the same values if their stored
-- values are the same and in the same order, regardless of the position of their heads.
-- 3. Define the two functions left and right, to move the position of the head on the left and on the right.
-- 4. Make Tape an instance of Functor and Applicative.



data Tape a = Tape [a] a [a] a

instance (Eq a) => Eq (Tape a) where
    (Tape l h r _) == (Tape x y z _) = (l ++ [h] ++ r) == (x ++ [y] ++ z)

instance (Show a) => Show (Tape a) where
    show (Tape x c y b) = show (reverse x) ++ show c ++ show y

left :: Tape a -> Tape a
left (Tape [] c y b) = Tape [] b (c : y) b
left (Tape (x : xs) c y b) = Tape xs x (c : y) b

right :: Tape a -> Tape a
right (Tape x c [] b) = Tape (c : x) b [] b 
right (Tape x c (y : ys) b) = Tape (c : x) y ys b

instance Functor Tape where
    fmap f (Tape x c y b) = Tape (fmap f x) (f c) (fmap f y) (f b)

instance Applicative Tape where
    pure x = Tape [] x [] x
    (Tape fx fc fy fb) <*> (Tape x c y b) = Tape (fx <*> x) (fc c) (fy <*> y) (fb b)

    


    
