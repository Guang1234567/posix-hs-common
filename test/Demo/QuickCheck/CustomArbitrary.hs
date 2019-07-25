{-# LANGUAGE InstanceSigs #-}

module Demo.QuickCheck.CustomArbitrary
    ( Doc
    , createDocTestData
    , prop_empty_id
    , runTests
    ) where

import Control.Monad
import Data.Monoid
import Prelude hiding ((<>))
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

data Doc
    = Empty
    | Char Char
    | Text String
    | Line
    | Concat Doc Doc
    | Union Doc Doc
    deriving (Show, Eq)

instance Arbitrary Doc where
    arbitrary :: Gen Doc
    arbitrary =
        oneof
            [ return Empty
            , fmap Char arbitrary
            , fmap Text arbitrary
            , return Line
            , liftM2 Concat arbitrary arbitrary
            , liftM2 Union arbitrary arbitrary
            ]

instance Semigroup Doc where
    (<>) :: Doc -> Doc -> Doc
    a <> b = append a b

instance Monoid Doc where
    mempty = empty

createDocTestData :: IO [Doc]
createDocTestData = sample' arbitrary

empty :: Doc
empty = Empty

append :: Doc -> Doc -> Doc
append Empty y = y
append x Empty = x
append x y = x `Concat` y

prop_empty_id :: Doc -> Bool
prop_empty_id x = (empty <> x) == x && (x <> empty) == x

prop_mempty_id :: Doc -> Bool
prop_mempty_id x = mempty `mappend` x == x && x `mappend` mempty == (x :: Doc)

runTests :: Args -> IO ()
runTests args = do
    f prop_empty_id "empty_id ok?"
    f prop_mempty_id "prop_mempty_id ok?"
  where
    f prop str = do
        putStrLn str
        quickCheckWithResult args prop
        return ()