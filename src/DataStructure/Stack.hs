module DataStructure.Stack
    ( Stack(..)
    , isIncreasing
    , push
    , pop
    , stackMap
    )
where

{-! 
    `栈`, LIFO (后进先出)

    let stack = Item 3 $ Item 2 $ Item 1 $ Bottom
-}
data Stack a = Bottom |  Item a (Stack a) deriving (Show)


isIncreasing :: (Ord a) => Stack a -> Bool
isIncreasing (Item top rest@(Item bottom _)) | top > bottom = isIncreasing rest
                                             | otherwise    = False
isIncreasing (Item _ Bottom) = True


push :: Stack a -> a -> Stack a
push bottomSt top = Item top bottomSt


pop :: Stack a -> Maybe (a, Stack a)
pop (Item top rest) = Just (top, rest)
pop _               = Nothing


top :: Stack a -> Maybe a
top (Item top _) = Just top
top _            = Nothing


stackMap :: (a -> b) -> Stack a -> Stack b
stackMap _ Bottom          = Bottom
stackMap f (Item top rest) = Item (f top) (stackMap f rest)


instance Functor Stack where
    fmap = stackMap
