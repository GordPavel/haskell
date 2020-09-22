module Lib
  ( fibonacci,
    sumNCount,
    seqA,
    integration,
    multSecond,
    on3,
    doItYourself,
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
