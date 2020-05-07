-- Welcome to the first exercise set of the Haskell Mooc! Edit this
-- file according to the instructions, and check your answers with
--
--   stack runhaskell Set1Test.hs
--
-- You can also play around with your answers in GHCi with
--
--   stack ghci Set1.hs
--
-- This set contains exercises on
--   * defining functions
--   * basic expressions
--   * pattern matching
--   * recursion


module Set1 where

import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: define variables one and two. They should have type Int and
-- values 1 and 2, respectively.


------------------------------------------------------------------------------
-- Ex 2: define the function double of type Integer->Integer. Double
-- should take one argument and return it multiplied by two.

double :: Integer -> Integer
double x = todo

------------------------------------------------------------------------------
-- Ex 3: define the function quadruple that uses the function double
-- from the previous exercise to return its argument multiplied by
-- four.

quadruple :: Integer -> Integer
quadruple x = todo

------------------------------------------------------------------------------
-- Ex 4: define the function distance. It should take four arguments of
-- type Double: x1, y1, x2, and y2 and return the (euclidean) distance
-- between points (x1,y1) and (x2,y2).
--
-- Give distance a type signature, i.e. distance :: something.
--
-- PS. if you can't remember how the distance is computed, the formula is:
--   square root of ((x distance) squared + (y distance) squared)
--
-- Examples:
--   distance 0 0 1 1  ==>  1.4142135...
--   distance 1 1 4 5  ==>  5.0

distance = todo

------------------------------------------------------------------------------
-- Ex 5: define the function eeny that returns "eeny" for even inputs
-- and "meeny" for odd inputs.
--
-- Ps. have a look at the built in function "even"

eeny :: Integer -> String
eeny = todo

------------------------------------------------------------------------------
-- Ex 6: here's the function checkPassword from the course material.
-- Modify it so that it accepts two passwords, "swordfish" and
-- "mellon".

checkPassword :: String -> String
checkPassword password = if password == "swordfish"
                         then "You're in."
                         else "ACCESS DENIED!"

------------------------------------------------------------------------------
-- Ex 7: A postal service prices packages the following way.
-- Packages that weigh up to 500 grams cost 250 credits.
-- Packages over 500 grams cost 300 credit + 1 credit per gram.
-- Packages over 5000 grams cost a constant 6000 credits.
--
-- Write a function postagePrice that takes the weight of a package
-- in, and returns the cost in credits.

postagePrice :: Int -> Int
postagePrice = todo

------------------------------------------------------------------------------
-- Ex 8: define a function isZero that returns True if it is given an
-- Integer that is 0, and False otherwise. Give isZero a type signature.
--
-- Use pattern matching! Don't use comparisons!
--
-- Ps. remember, the type of booleans in haskell is Bool

isZero = todo

------------------------------------------------------------------------------
-- Ex 9: implement using recursion a function sumTo such that
--   sumTo n
-- computes the sum 1+2+...+n

sumTo :: Integer -> Integer
sumTo = todo

------------------------------------------------------------------------------
-- Ex 10: power n k should compute n to the power k (i.e. n^k)
-- Use recursion.

power :: Integer -> Integer -> Integer
power = todo

------------------------------------------------------------------------------
-- Ex 11: ilog3 n should be the number of times you can divide given
-- number by three (rounding down) before you get 0.
--
-- For example, ilog3 20 ==> 3 since
--   20/3 = 6.66 (gets rounded down to 6)
--   6/3 = 2
--   2/3 = 0.666 (gets rounded down to 0)
--
-- Use recursion to define ilog3. Use the function "div" for integer
-- division. It rounds down for you.
--
-- More examples:
--   ilog3 2 ==> 1
--   ilog3 7 ==> 2

ilog3 :: Integer -> Integer
ilog3 = todo

------------------------------------------------------------------------------
-- Ex 12: compute binomial coefficients using recursion. Binomial
-- coefficients are defined by the following equations:
--
--   B(n,k) = B(n-1,k) + B(n-1,k-1)
--   B(n,0) = 1
--   B(0,k) = 0, when k>0
--
-- Hint! pattern matching is your friend.

binomial :: Integer -> Integer -> Integer
binomial = todo

------------------------------------------------------------------------------
-- Ex 13: implement the odd factorial function. Odd factorial is like
-- factorial, but it only multiplies odd numbers.
--
-- Examples:
--   oddFactorial 7 ==> 7*5*3*1 ==> 105
--   oddFactorial 6 ==> 5*3*1 ==> 15

oddFactorial :: Integer -> Integer
oddFactorial = todo

------------------------------------------------------------------------------
-- Ex 14: implement the Euclidean Algorithm for finding the greatest
-- common divisor:
--
-- Given two numbers, a and b,
-- * if one is zero, return the other number
-- * if not, subtract the smaller number from the larger one
-- * replace the larger number with this new number
-- * repeat
--
-- For example,
--   myGcd 9 12 ==> 3
-- In this case, the algorithm proceeds like this
--
--   a      b
--
--   9      12
--   9      (12-9)
--   9      3
--   (9-3)  3
--   6      3
--   (6-3)  3
--   3      3
--   (3-3)  3
--   0      3
--
-- Background reading:
-- * https://en.wikipedia.org/wiki/Euclidean_algorithm

myGcd :: Integer -> Integer -> Integer
myGcd = todo

------------------------------------------------------------------------------
-- Ex 15: Implement the function leftpad which adds space characters
-- to the start of the string until it is long enough.
--
-- Examples:
--   leftpad "foo" 5 ==> "  foo"
--   leftpad "13" 3 ==> " 13"
--   leftpad "xxxxx" 3 ==> "xxxxx"
--
-- Tips:
-- * you can combine strings with the ++ operator.
-- * you can compute the length of a string with the length function

leftpad :: String -> Int -> String
leftpad = todo

------------------------------------------------------------------------------
-- Ex 16: let's make a countdown for a rocket! Given a number, you
-- should produce a string that says "Ready!", counts down from the
-- number, and then says "Liftoff!".
--
-- For example,
--   countdown 4 ==> "Ready! 4... 3... 2... 1... Liftoff!"
--
-- Hints:
-- * you can combine strings with the ++ operator
-- * you can use the show function to convert a number into a string
-- * you'll probably need a recursive helper function

countdown :: Integer -> String
countdown = todo

------------------------------------------------------------------------------
-- Ex 17: implement the function smallestDivisor that returns the
-- smallest number (greater than 1) that divides the given number evenly.
--
-- That is, when
--   smallestDivisor n ==> k
-- we have
--   n = t*k
-- for some t.
--
-- Ps. your function doesn't need to work for inputs 0 and 1, but
-- remember this in the next exercise!

smallestDivisor :: Integer -> Integer
smallestDivisor = todo

------------------------------------------------------------------------------
-- Ex 18: implement a function isPrime that checks if the given number
-- is a prime number. Use the function smallestDivisor.
--
-- Ps. 0 and 1 are not prime numbers

isPrime :: Integer -> Bool
isPrime = todo

------------------------------------------------------------------------------
-- Ex 19: implement a function biggestPrimeAtMost that returns the
-- biggest prime number that is less than or equal to the given
-- number. Use the function isPrime you just defined.
--
-- Examples:
--   biggestPrimeAtMost 3 ==> 3
--   biggestPrimeAtMost 10 ==> 7

biggestPrimeAtMost :: Integer -> Integer
biggestPrimeAtMost = todo
