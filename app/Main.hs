--{-# LANGUAGE QuasiQuotes #-}
--{-# LANGUAGE TemplateHaskell #-}

module Main where

import Lib

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
  print x-}
