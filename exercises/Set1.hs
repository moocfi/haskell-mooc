module Set1 where

import Mooc.Todo

-- Ex 1
one :: Int
one = 1

two :: Int
two = 2

-- Ex 2
double :: Integer -> Integer
double x = x * 2

-- Ex 3
quadruple :: Integer -> Integer
quadruple x = double (double x)

-- Ex 4
distance :: Double -> Double -> Double -> Double -> Double
distance x1 y1 x2 y2 =
  sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

-- Ex 5
eeny :: Integer -> String
eeny x =
  if even x then "eeny" else "meeny"

-- Ex 6
checkPassword :: String -> String
checkPassword password =
  if password == "swordfish" || password == "mellon"
    then "You're in."
    else "ACCESS DENIED!"

-- Ex 7
postagePrice :: Int -> Int
postagePrice w
  | w <= 500 = 250
  | w <= 5000 = 300 + w
  | otherwise = 6000

-- Ex 8
isZero :: Integer -> Bool
isZero 0 = True
isZero _ = False

-- Ex 9
sumTo :: Integer -> Integer
sumTo 0 = 0
sumTo n = n + sumTo (n - 1)

-- Ex 10
power :: Integer -> Integer -> Integer
power _ 0 = 1
power n k = n * power n (k - 1)

-- Ex 11
ilog3 :: Integer -> Integer
ilog3 n
  | n <= 0 = 0
  | otherwise = 1 + ilog3 (n `div` 3)
