{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import Data.RedBlack.Internal

main :: IO ()
main = tests >> return ()

--
-- Notes: 'annotateShow' is useful for when a test case fails
--

--
-- Test that tree height is never over 2 * floor (logBase 2 (len + 1))
-- where len is number of elements in the tree.
--
prop_treesNotOverMaximumHeight :: Property
prop_treesNotOverMaximumHeight = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 1000) (Gen.int $ Range.linear 0 1000000)
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
  xs <- forAll $ Gen.list (Range.linear 0 1000) (Gen.int $ Range.linear 0 1000000)
  let tree = foldl (flip insert) E xs
  assert $ valid tree

data Op = Insert Int | Delete Int

--
-- Test that trees which have many values inserted and then a few of them
-- deleted are stil valid.
--
prop_treesAfterDeletionsAreValid :: Property
prop_treesAfterDeletionsAreValid = property $ do
  valuesToInsert <- forAll $ Gen.list (Range.linear 0 1000) (Gen.int $ Range.linear 0 1000000)
  let len = length valuesToInsert
  -- now delete 1/5th of them
  indicesToDelete <- forAll $ Gen.list (Range.singleton $ len `div` 5) (Gen.int $ Range.linear 0 (len-1))
  annotateShow len
  annotateShow indicesToDelete
  let treeAfterInsert = foldl (flip insert) E valuesToInsert
      treeAfterDelete = foldl (\t i -> delete (valuesToInsert !! i) t) treeAfterInsert indicesToDelete
  annotateShow treeAfterInsert
  annotateShow treeAfterDelete
  assert $ valid treeAfterDelete

tests :: IO Bool
tests = checkParallel $$(discover)