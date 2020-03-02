module Notes04 where

import Prelude hiding (Functor(..), Semigroup(..), Monoid(..))

infixr 6 <>
class Semigroup a where
    (<>) :: a -> a -> a
    -- has to be associative (not enforced by haskell)
    -- (x <> y) <> z == x <> (y <> z)

class Semigroup a => Monoid a where
    mempty :: a
    -- mempty <> x == x
    -- x <> mempty == x

instance Semigroup [a] where
    (<>) = (++)

instance Monoid [a] where
    mempty = []

mconcat :: Monoid a => [a] -> a
mconcat = foldr (<>) mempty

data BinTree a = Leaf a 
                | Node (BinTree a) (BinTree a)
                deriving (Eq, Show, Ord)

concatTree :: Semigroup a => BinTree a -> a
concatTree (Leaf a) = a
concatTree (Node l r) = (concatTree l) <> (concatTree r)

instance (Semigroup a, Semigroup b) => Semigroup (a,b) where
    (a, b) <> (a', b') = (a <> a', b <> b')

instance (Monoid a, Monoid b) => Monoid (a,b) where
    mempty = (mempty, mempty)

instance (Semigroup b) => Semigroup (a -> b) where
    (f <> g) x = (f x) <> (g x)

instance (Monoid b) => Monoid (a -> b) where
    mempty x = mempty
