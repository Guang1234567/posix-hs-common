module Lib
    ( someFunc
    )
where

import           Image.Netpbm.Parser
import qualified Data.ByteString.Lazy.Char8    as BLC8
import qualified Data.ByteString.Lazy          as BL

someFunc :: IO ()
someFunc =do
    let result = parse parseByte (BLC8.pack "9k fail")
    print result
