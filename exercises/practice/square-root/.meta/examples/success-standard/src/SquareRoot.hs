module SquareRoot (root) where

root :: Integer -> Integer
root n = head [num | num <- [1..n], num * num == n]