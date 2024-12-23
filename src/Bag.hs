{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Bag
  ( Bag (..),
    createBag,
    insert,
    remove,
    find,
    mapBag,
    filterBag,
    foldlBag,
    foldrBag,
  )
where

import qualified RBBag as RB

newtype Bag a = Bag (RB.RBBag a)
  deriving (Show, Eq)

instance (Ord a) => Semigroup (Bag a) where
  (<>) (Bag t1) (Bag t2) = Bag (t1 <> t2)

instance (Ord a) => Monoid (Bag a) where
  mempty = createBag

createBag :: Bag a
createBag = Bag RB.Leaf

insert :: (Ord a) => a -> Bag a -> Bag a
insert x (Bag tree) = Bag (RB.insert' x tree)

remove :: (Ord a) => a -> Bag a -> Bag a
remove x (Bag tree) = Bag (RB.remove' x tree)

find :: (Ord a) => a -> Bag a -> Maybe Int
find x (Bag tree) = RB.lookup' x tree

mapBag :: (Ord a) => (a -> a) -> Bag a -> Bag a
mapBag f (Bag tree) = Bag (RB.map' f tree)

filterBag :: (Ord a) => (Int -> Bool) -> Bag a -> Bag a
filterBag p (Bag tree) = Bag (RB.filter' (\(_, count) -> p count) tree)

foldlBag :: (Ord a) => ((a, Int) -> b -> b) -> b -> Bag a -> b
foldlBag f acc (Bag tree) = RB.foldl'' f acc tree

foldrBag :: (Ord a) => ((a, Int) -> b -> b) -> b -> Bag a -> b
foldrBag f acc (Bag tree) = RB.foldr'' f acc tree
