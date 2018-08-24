{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import Data.RedBlack.Internal

main :: IO ()
main = tests >> return ()

prop_test :: Property
prop_test = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int $ Range.linear 0 65536)
  let t = foldl (\t x -> insert x t) E xs
      len = length xs
  assert (height t <= 2*(floor $ logBase 2 (fromIntegral len)))


tests :: IO Bool
tests = checkParallel $$(discover)