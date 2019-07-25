module Algorithm.SortsSpec
    ( prop_idempotent
    , prop_minimum
    , prop_maximum
    , prop_permutation
    , prop_append_min
    , prop_append_max
    , runTests
    ) where

import Algorithm.Sorts

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Data.List

prop_idempotent :: [Integer] -> Bool
prop_idempotent xs = quickSort (quickSort xs) == quickSort xs

prop_minimum :: [Integer] -> Property
prop_minimum xs = (not . null) xs ==> head (quickSort xs) == minimum xs

prop_maximum :: [Integer] -> Property
prop_maximum xs = (not . null) xs ==> last (quickSort xs) == maximum xs

prop_permutation :: [Integer] -> Bool
prop_permutation xs = permutation xs (quickSort xs)
  where
    permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_append_min :: [Integer] -> [Integer] -> Property
prop_append_min xs ys =
    (not . null) xs ==> (not . null) ys ==> (head . quickSort) (xs ++ ys) == min (minimum xs) (minimum ys)

prop_append_max :: [Integer] -> [Integer] -> Property
prop_append_max xs ys =
    (not . null) xs ==> (not . null) ys ==> (last . quickSort) (xs ++ ys) == max (maximum xs) (maximum ys)

runTests :: Args -> IO ()
runTests args = do
    f prop_idempotent "idempotent ok?"
    f prop_minimum "minimum ok?"
    f prop_append_min "append_min ok?"
    f prop_append_max "append_max ok?"
  where
    f prop str = do
        putStrLn str
        quickCheckWithResult args prop
        return ()