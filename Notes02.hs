module Notes02 where

import Data.List

data Sum a b = Inj1 a 
    | Inj2 b
    deriving (Eq, Ord, Show)

-- instance (Eq a, Eq b) => Eq (Sum a b) where
--     Inj1 x == Inj1 y = x == y
--     Inj2 x == Inj2 y = x == y
--     _ == _ = False

-- peldak fv-kre amik typeclass constraintet hasznalnak 
-- ket rendezett lista -> rendezett lista
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y = x: merge xs (y:ys)
    | otherwise = y: merge (x:xs) ys

-- group' :: Eq a => [a] -> [[a]]
-- group [] = []
-- group' xs = [x] ++ group' ys
--     where
--         (x, ys) = go xs []
--         go [] acc = ([], acc)
--         go [x] acc = ([x], acc)
--         go (x:y:ys) acc
--             | x == y  = go (y:ys) (x:acc)
--             | otherwise = (y:ys, acc)

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort $ take n xs) (mergeSort $ drop n xs)
    where
        n = length xs `div` 2


group' :: Eq a => [a] -> [[a]]
group' [] = []
group' xs = a : group' b
    where
        (a,b) = spanEqual xs
        spanEqual [] = ([], [])
        spanEqual [x] = ([x], [])
        spanEqual (x:y:ys)
            | x == y = (x:a, b)
            | otherwise = ([x], y:ys)
            where (a,b) = spanEqual (y:ys)


data Tree a = Leaf a 
            | Node (Tree a) (Tree a)
            deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x)   = Leaf (f x)
mapTree f (Node l r) = Node (mapTree f l) (mapTree f r)

sumTree :: Num a => Tree a -> a
sumTree (Leaf x)   = x
sumTree (Node l r) = sumTree l + sumTree r

flattenTree :: Tree a -> [a]
flattenTree (Leaf x)   = [x]
flattenTree (Node l r) = flattenTree l ++ flattenTree r

fib = 1 : 1 : zipWith (+) fib (tail fib)

fibPowers = foldl' merge [] [[x ^ n | n<- [1..] ] | x <- drop 2 fib]