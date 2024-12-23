{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Monoid law, right identity" #-}
{-# HLINT ignore "Monoid law, left identity" #-}
module PropertyTestRBBag (properties) where

import RBBag
import Test.Tasty
import Test.Tasty.QuickCheck as QC

properties :: TestTree
properties =
  testGroup
    "RBBag properties-tests"
    [ QC.testProperty "Test monoid property" prop_monoid,
      QC.testProperty "Test monoid associativity" prop_monoid_assoc,
      QC.testProperty "Test insert increases count" prop_insert_count,
      QC.testProperty "Test color validation" prop_color_valid
    ]

instance (Ord a, Arbitrary a) => Arbitrary (RBBag a) where
  arbitrary = do
    xs <- listOf arbitrary
    return $ foldr insert' Leaf xs

prop_monoid :: RBBag Int -> Bool
prop_monoid m = (m <> mempty) == m && (mempty <> m) == m

prop_monoid_assoc :: RBBag Int -> RBBag Int -> RBBag Int -> Bool
prop_monoid_assoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

prop_insert_count :: Int -> RBBag Int -> Bool
prop_insert_count x tree = case lookup' x (insert' x tree) of
  Nothing -> False
  Just n -> n > 0

prop_color_valid :: RBBag Int -> Bool
prop_color_valid Leaf = True
prop_color_valid node@Node {color = Black} = go node
  where
    go Leaf = True
    go Node {color = Black, leftChild = left, rightChild = right} = go left && go right
    go Node {color = Red, leftChild = left@Node {color = Black}, rightChild = right@Node {color = Black}} = go left && go right
    go Node {color = Red, leftChild = left@Node {color = Black}, rightChild = Leaf} = go left
    go Node {color = Red, leftChild = Leaf, rightChild = right@Node {color = Black}} = go right
    go Node {color = Red, leftChild = Leaf, rightChild = Leaf} = True
    go _ = False
prop_color_valid _ = False
