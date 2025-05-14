module Lib
  ( hello
  , plus
  ) where

hello :: IO ()
hello = putStrLn "Hello, Nux!"

plus :: Int -> Int -> Int
plus a b = a + b