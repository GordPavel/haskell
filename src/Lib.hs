module Lib
  ( fibonacci,

  )
where

fibonacci :: Int -> Int
fibonacci = fib 0 1
  where
    fib a b n
      | n <= 1 = b
      | otherwise = fib b (a + b) (n -1)
