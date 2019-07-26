module Parser.Num
    ( SymbolicManip
    , prettyShow
    , simplify
    , rpnShow
    ) where

data Op
    = Plus
    | Minus
    | Mul
    | Div
    | Pow
    deriving (Eq, Show)

data SymbolicManip a
    = Number a
    | Symbol String
    | BinaryArith Op (SymbolicManip a) (SymbolicManip a)
    | UnaryArith String (SymbolicManip a)
    deriving (Eq)

instance Num a => Num (SymbolicManip a) where
    x + y = BinaryArith Plus x y
    x - y = BinaryArith Minus x y
    x * y = BinaryArith Mul x y
    negate = BinaryArith Mul (Number (-1))
    abs = UnaryArith "abs"
    signum _ = error "signum is unimplemented"
    fromInteger x = Number (fromInteger x)

instance (Fractional a) => Fractional (SymbolicManip a) where
    x / y = BinaryArith Div x y
    recip = BinaryArith Div (Number 1)
    fromRational r = Number (fromRational r)

instance (Floating a) => Floating (SymbolicManip a) where
    pi = Symbol "pi"
    exp = UnaryArith "exp"
    log = UnaryArith "log"
    sqrt = UnaryArith "sqrt"
    a ** b = BinaryArith Pow a b
    sin = UnaryArith "sin"
    cos = UnaryArith "cos"
    tan = UnaryArith "tan"
    asin = UnaryArith "asin"
    acos = UnaryArith "acos"
    atan = UnaryArith "atan"
    sinh = UnaryArith "sinh"
    cosh = UnaryArith "cosh"
    tanh = UnaryArith "tanh"
    asinh = UnaryArith "asinh"
    acosh = UnaryArith "acosh"
    atanh = UnaryArith "atanh"

prettyShow :: (Show a, Num a) => SymbolicManip a -> String
prettyShow (Number x) = show x
prettyShow (Symbol x) = x
prettyShow (BinaryArith op x y) =
    let x' = simpleParen x
        y' = simpleParen y
        op' = op2str op
     in x' ++ op' ++ y'
prettyShow (UnaryArith opStr x) = opStr ++ "(" ++ show x ++ ")"

{-
在需要的地方添加括号。这个函数比较保守，有时候不需要也会加。
Haskell 在构建 SymbolicManip 的时候已经处理好优先级了。
-}
simpleParen :: (Show a, Num a) => SymbolicManip a -> String
simpleParen x@BinaryArith {} = "(" ++ prettyShow x ++ ")"
simpleParen x = prettyShow x

op2str :: Op -> String
op2str Plus = "+"
op2str Minus = "-"
op2str Mul = "*"
op2str Div = "/"
op2str Pow = "**"

{- 调用 prettyShow 函数显示 SymbolicManip 值 -}
instance (Show a, Num a) => Show (SymbolicManip a) where
    show = prettyShow

{-
    以逆波兰的格式打印表达式, 用来跟 {@code prettyShow} 作比较
-}
rpnShow :: (Show a, Num a) => SymbolicManip a -> String
rpnShow =
    let toList (Number x) = [show x]
        toList (Symbol x) = [x]
        toList (BinaryArith op x y) = toList x ++ toList y ++ [op2str op]
        toList (UnaryArith opstr x) = toList x ++ [opstr]
     in unwords . toList

{-
    简化表达式用的
    如:
      > prettyShow $ simplify $ 5 + 1 * 3
      < "5+3"
-}
simplify :: (Eq a, Num a) => SymbolicManip a -> SymbolicManip a
simplify (BinaryArith op x y) =
    let x' = simplify x
        y' = simplify y
     in case (op, x', y') of
            (Mul, Number 1, b) -> b
            (Mul, a, Number 1) -> a
            (Mul, Number 0, b) -> Number 0
            (Mul, a, Number 0) -> Number 0
            (Div, a, Number 1) -> a
            (Plus, a, Number 0) -> a
            (Plus, Number 0, b) -> b
            (Minus, a, Number 0) -> a
            _ -> BinaryArith op x' y'
simplify (UnaryArith op a) = UnaryArith op (simplify a)
simplify x = x