{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser.Function
    (
   {- GenTypeArbitrary(..)
    , GenType
    , asTypeStr
    , test1
    , test2
    , test3
    , test4-}
    ) where

import Data.List
import Data.Typeable

instance (Typeable a, Typeable b) => Show (a -> b) where
    show _ = show $ typeOf (undefined :: a -> b)

{-
-- 下面是等价的方式

newtype GenType a =
    GenType
        { asTypeStr :: String
        }

class GenTypeArbitrary a where
    gtArbitrary :: a -> GenType a

instance GenTypeArbitrary String where
    gtArbitrary :: String -> GenType String
    gtArbitrary _ = GenType "String123"

instance GenTypeArbitrary Bool where
    gtArbitrary :: Bool -> GenType Bool
    gtArbitrary _ = GenType "Bool123"

instance GenTypeArbitrary Int where
    gtArbitrary :: Int -> GenType Int
    gtArbitrary _ = GenType "Int123"

instance (GenTypeArbitrary a, GenTypeArbitrary b) => GenTypeArbitrary (a -> b) where
    gtArbitrary :: (GenTypeArbitrary a, GenTypeArbitrary b) => (a -> b) -> GenType (a -> b)
    gtArbitrary _ = GenType $ aTypeStr' ++ " --> " ++ bTypeStr
      where
        aTypeStr = asTypeStr (gtArbitrary (undefined :: a))
        aTypeStr' =
            if "-->" `isInfixOf` aTypeStr
                then "(" ++ aTypeStr ++ ")"
                else aTypeStr
        bTypeStr = asTypeStr (gtArbitrary (undefined :: b))

instance  (GenTypeArbitrary a, GenTypeArbitrary b) => Show (a -> b) where
  show f = asTypeStr $ gtArbitrary f

test1 :: Int -> String
test1 x = ""

test2 :: Int -> String -> Int -> Bool -> Bool
test2 _ _ _ _ = False

test3 :: Int -> ((String -> Int) -> Bool) -> Bool
test3 _ _ = False

test4 :: Int -> (Bool -> (String -> Int)) -> Bool
test4 _ _ = False
---}