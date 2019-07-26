{-# LANGUAGE ScopedTypeVariables #-}

module DataStructure.Array
    ( foldArrayL'
    ) where

import           Data.Array

-- | Strict left fold for {@code Array}, similar to foldl' on {@code List}
-- ghci :
--  > let a = Array.listArray (0, 3) [1..]
--  < array (0, 3) [(0, 1), (1, 2), (2, 3), (3, 4)]
--  > foldArrayL' (+) 0 a
--  < 10
foldArrayL' ::
       forall a b k. (Ix k)
    => (a -> b -> a)
    -> a
    -> Array k b
    -> a
foldArrayL' f acc arr = foldByIndex acc (indices arr)
  where
    foldByIndex :: a -> [k] -> a
    foldByIndex acc (idx:idxs) =
        let acc' = f acc (arr ! idx)
         in acc' `seq` foldByIndex acc' idxs
    foldByIndex acc _ = acc

--foldArrayL'' :: Ix k => (a -> a -> a) -> Array k a -> a
--foldArrayL'' f arr = foldArrayL' f (arr ! fst (bounds arr)) xs
