{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LiberalTypeSynonyms #-}

{- |
netpbm 的灰度文件格式名为 PGM（”portable grey map”）。
事实上它不是一个格式，而是两个： 纯文本（又名P2）格式使用 ASCII 编码，而更常用的原始（P5）格式则采用二进制表示。
每种文件格式都包含头信息，头信息以一个“魔法”字符串开始，指出文件格式。
纯文本格式是 P2，原始格式是 P5。
魔法字符串之后是空格，然后是三个数字：宽度、高度、图像的最大灰度值。这些数字用十进制 ASCII 数字表示，并用空格隔开。

最大灰度值之后便是图像数据了。在原始文件中，这是一串二进制值。纯文本文件中，这些值是用空格隔开的十进制 ASCII 数字。
原始文件可包含多个图像，一个接一个，每个都有自己的头信息。
纯文本文件只包含一个图像。
-}
module Image.Netpbm.Parser
    ( Parser
    , ParserWrapper(..)
    , ParseState(..)
    , identity
    , parseByte
    , parseChar
    , peekByte
    , peekChar
    , parse
    )
where

import           Control.Exception

import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BLC8
import           Data.Char
import           Data.Function
import           Data.Int
import           Data.Typeable
import           Data.Word

import           Utils.Preclude

{- |
我们用一个直白的数据类型来表示 PGM 图像
-}
data GreyMap =
    GreyMap
        { greyWidth :: Int
        , greyHeight :: Int
        , greyMax :: Int
        , greyData :: BL.ByteString
        }
    deriving (Eq)

{- |
类似于重写 java 的 Object::toString 方法
-}
instance Show GreyMap
    {- 最后一个参数是 greyData, 这里用 `_` 占位符来代替, 这是因为我们不会把它打印出来 -}
                                                           where
    show (GreyMap width height max _) =
        "GreyMap " ++ show width ++ "x" ++ show height ++ " " ++ show max

matchHeader :: BL.ByteString -> BL.ByteString -> Maybe BL.ByteString
matchHeader prefix str
    | prefix `BLC8.isPrefixOf` str = Just
        (BLC8.dropWhile isSpace (BL.drop (BL.length prefix) str))
    |
      -- 去掉 `prefix` 和紧挨着的空格
      otherwise = Nothing

-- | "nat" here is short for "natural number"
getNat :: BL.ByteString -> Maybe (Int, BL.ByteString)
getNat s = case BLC8.readInt s of
    Nothing -> Nothing
    Just (num, rest) | num <= 0  -> Nothing
                     | otherwise -> Just (num, BLC8.dropWhile isSpace rest)

getBytes :: Int -> BL.ByteString -> Maybe (BL.ByteString, BL.ByteString)
getBytes n str =
    let count            = fromIntegral n
        both@(prefix, _) = BL.splitAt count str
    in  if BL.length prefix < count then Nothing else Just both

{- |
p5 格式解析器
-}
parseP5' :: BL.ByteString -> Maybe (GreyMap, BL.ByteString)
parseP5' s = case matchHeader (BLC8.pack "P5") s of
    Nothing -> Nothing
    Just s1 -> case getNat s1 of
        Nothing          -> Nothing
        Just (width, s2) -> case getNat (BLC8.dropWhile isSpace s2) of
            Nothing           -> Nothing
            Just (height, s3) -> case getNat (BLC8.dropWhile isSpace s3) of
                Nothing -> Nothing
                Just (greyMax, s4)
                    | greyMax > 255 -> Nothing
                    | otherwise -> case getBytes 1 s4 of
                        Nothing      -> Nothing
                        Just (_, s5) -> case getBytes (width * height) s5 of
                            Nothing -> Nothing
    -- `P5` 格式的 `greyData` 部分可包含多个图像，一个接一个.  `s6 就是剩余还没解析的部分`
                            Just (bitmap, s6) ->
                                Just (GreyMap width height greyMax bitmap, s6) -- `s5` 就是 `greyData` 部分 -- 这一个字节还不知道到底是什么

(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v  >>? f = f v

parseP5'' :: BL.ByteString -> Maybe (GreyMap, BL.ByteString)
parseP5'' s = matchHeader (BLC8.pack "P5") s >>? getNat >>? \(width, s2) ->
    getNat s2 >>? \(height, s3) ->
        getNat s3 >>? checkGreyMax >>? \(greyMax, s4) ->
            getBytes 1 s4
                >>? (snd |> getBytes (width * height))
                >>? \(bitmap, s6) ->
                        Just (GreyMap width height greyMax bitmap, s6)
  where
    checkGreyMax :: (Int, BL.ByteString) -> Maybe (Int, BL.ByteString)
    checkGreyMax (greyMax, s) | greyMax > 255 = Nothing
                              | otherwise     = Just (greyMax, s)

data ParseState =
    ParseState
        { string :: BL.ByteString
        , offset :: Int64
        }
    deriving (Show, Typeable)

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset = initState { offset = newOffset }

{- | 定义一个函数类型, 表示一个`解析器`的概念-}
type Parser a b = ParseState -> Either a (b, ParseState)

instance {-# OVERLAPPING #-} Show (Parser String Char) where
    show :: Parser String Char -> String
    show f = "<function> :: " ++ show (typeOf (f :: Parser String Char))

instance {-# OVERLAPPABLE #-} Show (Parser a b) where
    show :: Parser a b -> String
    show f = "<function> :: ParseState -> Either a (b, ParseState)"

newtype ParserWrapper a b =
    ParserWrapper
        { getParser :: Parser a b
        }

instance Show (ParserWrapper a b) where
    show (ParserWrapper f) = "ParserWrapper { " ++ show f ++ " }"

instance Functor (ParserWrapper a) where
    fmap f (ParserWrapper parser) =
        ParserWrapper (parser ==> \result -> identity (f result))

instance {-# OVERLAPPING #-} Functor ((->) ParseState) where
    fmap = (.)



{- |
    原样返回 `Parser a b`.
例如:
    `identity 1` 的类型是  `Parser String Int` 同时也是 `ParseState -> Either String (Int, ParseState)`
运行:
    > let f = `identity 1`
    > f (ParseState undefined 0)

    < Right (1, ParseState undefined 0)

    ----------------------------------------

    > let f = `identity "foo"`
    > f (ParseState undefined 0)

    < Right ("foo", ParseState undefined 0)
-}
identity :: a -> Parser b a
identity v ps = Right (v, ps)

{- |
    组合多个 `Parser`
-}
(==>) :: Parser a b -> (b -> Parser a c) -> Parser a c
firstParser ==> secondParser = \initState -> case firstParser initState of
    Left  err                       -> Left err
    Right (firstResult, firstState) -> secondParser firstResult firstState

{- |
    `Parser a b` : 传一个`解析器`, 如上面的 `identity 1`
-}
parse :: (Show a) => Parser a b -> BL.ByteString -> Either a b
parse f initState = case f (ParseState initState 0) of
    Left  err          -> Left err
    Right (result, ps) -> Right result

{- |
    获取`解析器`的运行结果, 最终结果的类型为 `ParseState` (解析状态)
-}
getState :: Parser String ParseState
getState ps = Right (ps, ps)

{- |
    用目前的 `ParseState` (解析状态) 替换为一个新的解析状态
-}
putState :: ParseState -> Parser String ()
putState ps _ = Right ((), ps)

bail :: String -> Parser String a
bail err ps = Left ("byte offset " ++ show (offset ps) ++ ": " ++ err)

{- |
运行:
  > let parser = parseByte
  > parse parser (BLC8.pack "ok fail")

  < Right '102'

函数解析:
    1) `getState` 获取当前的解析状态, 当前的解析状态(`initState`)是  `ParseState (BLC8.pack "ok fail") 0`   请看 `parse` 的函数实现.
    2) 把 `ParseState (BLC8.pack "ok fail") 0` 的 `(BLC8.pack "ok fail")` 取出来, 并分割成`第一个字节和剩余部分`
-}
parseByte :: Parser String Word8
parseByte = getState ==> \initState -> case BL.uncons (string initState) of
    Nothing                -> bail "no more input"
    Just (byte, remainder) -> putState newState ==> \_ -> identity byte
      where
        newState  = initState { string = remainder, offset = newOffset }
        newOffset = offset initState + 1

parserMap :: (a -> b) -> Parser e a -> Parser e b
parserMap f p = getParser (f <$> ParserWrapper p)

parseBytes :: Int -> Parser String [Word8]
parseBytes 0 = identity []
parseBytes n = parseByte ==> \b -> parserMap (b :) (parseBytes (n - 1))

{-|
由于 `Parser a b` 是 `Functor` 的 instance.
故可以通过 `fmap` 的方式重复利用 `parseByte` 生成 `parseChar`
------------------------------------------
Functor Law:
    fmap id       ==  id
    fmap (f . g)  ==  fmap f . fmap g
------------------------------------------
运行:
  > let parser = parseChar
  > parse parser (BLC8.pack "ok fail")

  < Right 'f'
-}
parseChar :: Parser String Char
-- parseChar = (word8ToChar8 <$> parseByte)
-- parseChar = getParser ( chr <$> fromIntegral <$> ParserWrapper parseByte)
parseChar = parserMap word8ToChar8 parseByte

{-|
窥视第一个字节
-----------------------------------------
运行:
  > let parser = peekByte
  > parse parser (BLC8.pack "ok fail")

  < Right '102'
-}
peekByte :: Parser String (Maybe Word8)
peekByte = parserMap (fmap fst . BL.uncons . string) getState

peekChar :: Parser String (Maybe Char)
-- peekChar = getParser (fmap (word8ToChar8 . fst) . BL.uncons . string <$> ParserWrapper getState)
peekChar = parserMap (fmap word8ToChar8) peekByte

parseWhile' :: (Word8 -> Bool) -> Parser String [Word8]
parseWhile' f = parserMap (fmap f) peekByte ==> appendByte
  where
    appendByte :: Maybe Bool -> Parser String [Word8]
    appendByte (Just True) =
        parseByte ==> \b -> parserMap (b :) (parseWhile' f)
    appendByte Nothing = identity []

parseWhile :: (Word8 -> a) -> (a -> Bool) -> Parser String [a]
parseWhile f g = parserMap (fmap f) (parseWhile' (g . f))

(==>&) :: Parser a b -> Parser a c -> Parser a c
p ==>& f = p ==> const f

skipSpaces :: Parser String ()
skipSpaces = parseWhile word8ToChar8 isSpace ==>& identity ()

assert' :: Bool -> String -> Parser String ()
assert' True  _   = identity ()
assert' False err = bail err

parseNat :: Parser String Int
parseNat = parseWhile word8ToChar8 isDigit ==> \digits -> if null digits
    then bail "no more input"
    else
        let n = read digits
        in  if n < 0 then bail "integer overflow" else identity n

--parseP5 :: BL.ByteString -> Maybe (GreyMap, BL.ByteString)
parseP5 = parseWhile word8ToChar8 notWhite ==> \header ->
    skipSpaces
        ==>& assert' (header == "P5") "invalid raw header"
        ==>& parseNat
        ==>  \width -> skipSpaces ==>& parseNat ==> \height ->
                 skipSpaces ==>& parseNat ==> \maxGrey ->
                     parseByte
                         ==>& parseBytes (width * height)
                         ==>  \bitmap ->
                                  identity
                                      (GreyMap width
                                               height
                                               maxGrey
                                               (BL.pack bitmap)
                                      )
  where
    notWhite :: Char -> Bool
    notWhite = (`notElem` (" \r\n\t" :: String))
