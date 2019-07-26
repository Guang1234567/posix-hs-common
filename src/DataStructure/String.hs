module DataStructure.String
    ( split
    ) where

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = [[]]
split delimiter str =
    let (before, remainder) = span (/= delimiter) str
     in before :
        case remainder of
            [] -> []
            x  -> split delimiter (tail remainder)
