module Lib
  ( fibonacci,
    sumNCount,
    seqA,
    integration,
    multSecond,
    on3,
    doItYourself,
    Printable,
    MyIp
  )
where

import Data.Char
import Data.Function

fibonacci :: Int -> Int
fibonacci = fib 0 1
  where
    fib a b n
      | n <= 1 = b
      | otherwise = fib b (a + b) (n -1)

seqA :: Int -> Int
seqA = seqA' 3 2 1
  where
    seqA' first second third n
      | n == 2 = first
      | n == 1 = second
      | n == 0 = third
      | otherwise = seqA' (first + second - 2 * third) first second (n -1)

sumNCount :: Int -> (Int, Int)
sumNCount x =
  let arr = show $ abs x
   in (sum $ map digitToInt arr, length arr)

integration :: (Eq t, Fractional t) => t -> (t -> t) -> t -> t -> t
integration steps f a b =
  let step = (b - a) / steps
      trapezoidSquare k l h = (k + l) * h / 2
      integration' accum left n
        | n == steps = accum
        | otherwise = integration' (accum + trapezoidSquare (f left) (f (left + step)) step) (left + step) (n + 1)
   in integration' 0 a 0

multSecond :: (a, Integer) -> (a, Integer) -> Integer
multSecond = g `on` h
  where
    g = (*)
    h = snd

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

doItYourself :: Double -> Double
doItYourself = f . g . h
  where
    f = logBase 2.0
    g = flip (^) 3
    h = max 42

class Printable a where
  toString :: a -> String

instance Printable Bool where
  toString True = "true"
  toString False = "false"

instance Printable () where
  toString _ = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
  toString a = "(" ++ toString (fst a) ++ "," ++ toString (snd a) ++ ")"

class KnownToGork a where
  stomp :: a -> a
  doesEnrageGork :: a -> Bool

class KnownToMork a where
  stab :: a -> a
  doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
  stompOrStab :: a -> a
  stompOrStab b
  {-|
                                                 | doesEnrageGork b && doesEnrageMork b = stomp (stab b)
    Why does idea's haskell plugin show error at ^

    <q name>, <varsym>, HaskellTokenType.BACKQUOTE, HaskellTokenType.CHARACTER_LITERAL, HaskellTokenType.COLON_COLON, 
    HaskellTokenType.CONSYM_ID, HaskellTokenType.CON_ID, HaskellTokenType.DECIMAL, HaskellTokenType.DIRECTIVE, 
    HaskellTokenType.DOUBLE_RIGHT_ARROW, HaskellTokenType.FLOAT, HaskellTokenType.FORALL, HaskellTokenType.HEXADECIMAL, 
    HaskellTokenType.INCLUDE_DIRECTIVE, HaskellTokenType.LEFT_BRACE, HaskellTokenType.LEFT_BRACKET, HaskellTokenType.LEFT_PAREN, 
    HaskellTokenType.NEWLINE, HaskellTokenType.OCTAL, HaskellTokenType.PRAGMA_START, HaskellTokenType.QUOTE, 
    HaskellTokenType.RIGHT_ARROW, HaskellTokenType.SEMICOLON, HaskellTokenType.STRING_LITERAL, HaskellTokenType.TILDE, 
    HaskellTokenType.UNDERSCORE, HaskellTokenType.VAR_ID or forall expected, got '|'
  -}
    | doesEnrageGork b && doesEnrageMork b = stomp (stab b)
    | doesEnrageMork b = stomp b
    | doesEnrageGork b = stab b
    | otherwise = b
    
    
newtype MyIp = MyIp Int
instance Show MyIp where
  show (MyIp a) = show a ++ "."

ip = show a ++ show b ++ show c ++ show d
  where
    a = MyIp 127
    b = MyIp 224
    c = MyIp 120
    d = 12

