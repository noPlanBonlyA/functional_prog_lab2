module TestBag (tests) where

import Bag
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Bag unit-tests"
    [ testCase "Create bag" testCreate,
      testCase "Insert elements" testInsert,
      testCase "Multiple insertion" testInsertMultiple,
      testCase "Remove element" testRemove,
      testCase "Remove with count" testRemoveWithCount,
      testCase "Map with increment" testMapInc,
      testCase "Filter elements" testFilter,
      testCase "Fold elements" testFold
    ]

testCreate :: Assertion
testCreate = do
  let bag = createBag :: Bag Int
  bag @?= createBag

testInsert :: Assertion
testInsert = do
  let bag = insert 1 (createBag :: Bag Int)
  find 1 bag @?= Just 1

testInsertMultiple :: Assertion
testInsertMultiple = do
  let bag = insert 1 $ insert 1 $ insert 2 createBag
  find 1 bag @?= Just 2
  find 2 bag @?= Just 1

testRemove :: Assertion
testRemove = do
  let bag = insert 1 $ insert 1 $ insert 2 createBag
  let bag' = remove 1 bag
  find 1 bag' @?= Nothing
  find 2 bag' @?= Just 1

testRemoveWithCount :: Assertion
testRemoveWithCount = do
  let bag = insert 1 $ insert 1 createBag
  let bag' = remove 1 bag
  find 1 bag' @?= Nothing

testMapInc :: Assertion
testMapInc = do
  let bag = insert 1 $ insert 1 $ insert 2 createBag
  let bag' = mapBag (+1) bag
  find 2 bag' @?= Just 2
  find 3 bag' @?= Just 1

testFilter :: Assertion
testFilter = do
  let bag = insert 1 $ insert 1 $ insert 2 createBag
  putStrLn $ "Original bag: " ++ show bag
  let bag' = filterBag (\count -> count > 1) bag
  putStrLn $ "Filtered bag: " ++ show bag'
  find 1 bag' @?= Just 2
  find 2 bag' @?= Nothing

testFold :: Assertion
testFold = do
  let bag = insert 1 $ insert 1 $ insert 2 createBag
  let sum = foldlBag (\(_, count) acc -> acc + count) 0 bag
  sum @?= 3
  