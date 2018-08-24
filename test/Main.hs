{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import           Data.List (nub, (\\))

-- friends
import Data.RedBlack.Internal

{-
   Notes: 'annotateShow' is useful for when a test case fails
-}


main :: IO ()
main = tests >> return ()

genValuesToInsert :: Gen [Int]
genValuesToInsert = Gen.list (Range.linear 100 1000) (Gen.int $ Range.linear 0 1000000)

--
-- Test that tree height is never over 2 * floor (logBase 2 (len + 1))
-- where len is number of elements in the tree.
--
prop_treesNotOverMaximumHeight :: Property
prop_treesNotOverMaximumHeight = property $ do
  xs <- forAll $ genValuesToInsert
  let t = foldl (flip insert) E xs
      len = length xs
      theHeight = height t
      expectedHeight = 2 * (floor $ logBase 2 (fromIntegral len + 1))
  annotateShow len
  annotateShow theHeight
  annotateShow expectedHeight
  assert $ theHeight <= expectedHeight

--
-- Test that trees which have values inserted into them (with no deletions)
-- are valid
--
prop_treesInsertOnlyAreValid :: Property
prop_treesInsertOnlyAreValid = property $ do
  xs <- forAll $ genValuesToInsert
  let tree = foldl (flip insert) E xs
  assert $ valid tree

data Op = Insert Int | Delete Int

--
-- Test that trees which have many values inserted and then a few of them
-- deleted are stil valid.
--
prop_treesAfterDeletionsAreValid :: Property
prop_treesAfterDeletionsAreValid = property $ do
  valuesToInsert <- forAll $ genValuesToInsert
  let len = length valuesToInsert
  -- now delete 1/5th of them
  indicesToDelete <- forAll $ Gen.list (Range.singleton $ len `div` 5) (Gen.int $ Range.linear 0 (len-1))
  annotateShow len
  annotateShow indicesToDelete
  let treeAfterInsert = foldl (flip insert) empty valuesToInsert
      treeAfterDelete = foldl (\t i -> delete (valuesToInsert !! i) t) treeAfterInsert indicesToDelete
  annotateShow treeAfterInsert
  annotateShow treeAfterDelete
  assert $ valid treeAfterDelete

prop_memberReturnsTrueForInsertedValues :: Property
prop_memberReturnsTrueForInsertedValues = property $ do
  valuesToInsert <- forAll $ genValuesToInsert
  let tree = foldl (flip insert) empty valuesToInsert
  assert $ all (flip member tree) valuesToInsert

prop_memberReturnsFalseForValuesNotInserted :: Property
prop_memberReturnsFalseForValuesNotInserted = property $ do
  valuesToInsert <- nub <$> (forAll $ genValuesToInsert)
  moreValues <- forAll $ genValuesToInsert
  let valuesNotInserted = nub moreValues \\ nub valuesToInsert
  annotateShow valuesToInsert
  annotateShow valuesNotInserted
  let tree = foldl (flip insert) empty valuesToInsert
  assert $ all (not . flip member tree) valuesNotInserted

tests :: IO Bool
tests = checkParallel $$(discover)