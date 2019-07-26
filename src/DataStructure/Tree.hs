{-# LANGUAGE ScopedTypeVariables #-}

module DataStructure.Tree
    ( treeMap
    , treeLength
    , Tree(..)
    ) where

data Tree a
    = Node
          { lhs :: Tree a
          , rhs :: Tree a
          }
    | Leaf a
    deriving (Show)

treeLength' (Leaf s) = Leaf $ length s
treeLength' (Node l r) = Node (treeLength' l) (treeLength' r)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf s) = Leaf $ f s
treeMap f (Node l r) = Node (treeMap f l) (treeMap f r)

treeLength'' :: (Foldable t) => Tree (t a) -> Tree Int
treeLength'' = treeMap length

instance Functor Tree where
    fmap = treeMap

treeLength :: (Foldable t) => Tree (t a) -> Tree Int
treeLength = fmap length