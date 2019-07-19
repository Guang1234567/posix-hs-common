{-# LANGUAGE ScopedTypeVariables #-}

module Utils.Preclude
    ( maybeIO
    , eitherIO
    , maybeToEither
    , eitherToMaybe
    , (|>)
    , word8ToChar8
    ) where

import Control.Exception
import Control.Monad

import Data.Char
import Data.Time.Clock (UTCTime(..))

import Foreign
import Foreign.C

import System.Directory
import System.FilePath
import System.Posix.Files

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle fallback $ fmap Just act
  where
    fallback (_ :: SomeException) = return Nothing

eitherIO :: (Eq a) => a -> IO b -> IO (Either a b)
eitherIO left act = handle fallback $ fmap Right act
  where
    fallback (_ :: SomeException) = return $ Left left

{- |
-----------------------------------------------------------------
解析过程:
    maybe :: b -> (a -> b) -> Maybe a -> b
    flip maybe :: (a -> b) -> b -> Maybe a -> b
    flip maybe Right :: Either l a -> (Maybe a -> Either l a)
    (.) :: (b -> c) -> (a -> b) -> a -> c
    flip maybe Right . Left :: l -> Maybe a -> Either l a
-}
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither = flip maybe Right . Left

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

{-|
Map each element of a structure to a monadic action, evaluate
these actions from left to right, and collect the results. For
a version that ignores the results see 'Data.Foldable.mapM_'.
-}
(|>) :: (a -> b) -> (b -> c) -> a -> c
(|>) = flip (.)

infixr 9 |>

word8ToChar8 :: Word8 -> Char
word8ToChar8 = chr . fromIntegral