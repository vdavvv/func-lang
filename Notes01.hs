module Notes01 where
import Prelude hiding (showList)

-- ghci commands
-- :t
-- :r
-- :i

eqBool :: Bool -> Bool -> Bool
eqBool True  True  = True
eqBool False False = True
eqBool _ _ = False

-- eqList :: (a->a->Bool) -> [a] -> [a] -> Bool
-- eqList eqA [] [] = True
-- eqList eqA (x:xs) (y:ys) = (eqA x y) && eqList xs ys
-- eqList _ _ = False

-- eqBoolList :: [Bool] -> [Bool] -> Bool
-- eqBoolList = eqList eqBool

-- eqListListBool :: [[Bool]] -> [[Bool]] -> Bool
-- eqListListBool = eqList eqBoolList

--type classes
class Eq' a where
    eq :: a -> a -> Bool

instance Eq' Bool where
    eq = eqBool

eqList :: Eq' a => [a] -> [a] -> Bool
eqList []     []     = True
eqList (x:xs) (y:ys) = (eq x y) && eqList xs ys
eqList _      _      = False

instance Eq' a => Eq' [a] where
    eq = eqList

instance (Eq' a, Eq' b) => Eq' (a, b) where
    eq (a1, b1) (a2, b2)  = (eq a1 a2) && (eq b1 b2)



--group' :: Eq' a => [a] -> [[a]]
group' [] = []
group' (x:xs) = let (a,b) = span (eq x) (x:xs) 
                in a: (group' b)

-- span :: (a->Bool) -> [a] -> ([a],[a])
-- span 
    
data BinTree a = Leaf a
                | Node (BinTree a) (BinTree a) 


tree :: BinTree Int 
tree = Node (Node (Leaf 2) (Leaf 4)) (Leaf 5)

instance Show a => Show (BinTree a) where
    show (Leaf x) = show x
    show (Node l r) = "(" ++ show l ++ "," ++ show r ++ ")"

instance Eq a => Eq (BinTree a) where
    (Leaf x) == (Leaf y) = x == y
    (Node l1 r1) == (Node l2 r2) = l1 == l2 && r1 == r2

--instance Ord a => Ord (BinTree a) where

--optional homework
-- Eq, Ord instances for BinTree