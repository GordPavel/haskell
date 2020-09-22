module Lib
  ( fibonacci,
    sumNCount,
    seqA,
  )
where

import Data.Char

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
