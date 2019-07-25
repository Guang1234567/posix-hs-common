--{-# LANGUAGE QuasiQuotes #-}
--{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}

module Main
    ( main
    {-
    , funcWrapperUse
    , Example(..)
    , FuncWrapper(..)
    , TestData(..)
    -}
    )
where

import           Lib

main :: IO ()
main = someFunc


{-

import qualified Language.C.Inline as C

C.include "<math.h>"

main :: IO ()
main = do
  x <-
  [C.exp| double{
   cos(0)
   }|]
  print x

---}


{-

class Example e where
    thingy :: a -> b -> e a b

-- legit, but awkward
newtype FuncWrapper e a b =
    FuncWrapper
        { apply :: a -> e a b
        }

data TestData a b =
    TestData a b
    deriving (Show)

instance (Example e) => Example (FuncWrapper e) where
    thingy :: a -> b -> FuncWrapper e a b
    thingy _ = FuncWrapper . flip thingy

instance Example TestData where
    thingy :: a -> b -> TestData a b
    thingy = TestData

funcWrapperUse :: (Example e) => e Int String
funcWrapperUse = thingy 1 "two" `apply` 3 `apply` 4 `apply` 5

---}
