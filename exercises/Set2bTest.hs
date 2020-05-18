{-# LANGUAGE TemplateHaskell #-}

module Set2bTest where

import Data.List
import Test.QuickCheck

import Mooc.Th
import Mooc.Test

import Set2b

main = score tests

tests = [(1,"binomial",[ex1_binomial])
        ,(2,"oddFactorial",[ex2_oddFactorial])
        ,(3,"myGcd",[ex3_myGcd])
        ,(4,"leftpad",[property ex4_leftpad])
        ,(5,"countdown",[ex5_countdown])
        ,(6,"smallestDivisor",[ex6_smallestDivisor_prime, property ex6_smallestDivisor_comp])
        ,(7,"isPrime",[ex7_isPrime])
        ,(8,"biggestPrimeAtMost",[ex8_biggestPrimeAtMost])]

ex1_binomial =
  forAllBlind (elements [0..10]) $ \n ->
  forAllBlind (elements [0..n]) $ \k ->
  $(testing [|binomial n k|]) (?== f n `div` (f k * f (n-k)))
  where f n = product [1..n]

ex2_oddFactorial =
  forAllBlind (elements [1..15]) $ \n ->
  $(testing [|oddFactorial n|]) (?== f n)
  where f n = product [1,3..n]

ex3_myGcd =
  forAllBlind (elements [1..max]) $ \x ->
  forAllBlind (elements [1..max]) $ \y ->
  $(testing [|myGcd x y|]) (?== gcd x y)
  where max = 10000

word = listOf1 (choose ('a','z'))

ex4_leftpad =
  do text <- word
     ws <- listOf (return ' ')
     let res = ws++text
     let len = length res
     return $ $(testing [|leftpad text len|]) (?==res)

ex5_countdown =
  forAllBlind (elements [1..20]) $ \n ->
  $(testing [|countdown n|]) . was $ \v ->
  counterexample "should start with \"Ready!\"" (take 6 v ?== "Ready!")
  .&&.
  counterexample "should end with \"Liftoff!\"" (reverse (take 8 (reverse v)) ?== "Liftoff!")
  .&&.
  let cnt = init $ init $ intercalate "... " (map show [n,n-1..0])
  in counterexample ("should contain the string " ++ show cnt) (isInfixOf cnt v)

primes = go [2..]
  where go (x:xs) = x : go (filter (notDivBy x) xs)
        notDivBy x y = mod y x /= 0

ex6_smallestDivisor_prime = do
  forAllBlind (elements $ take 12 primes) $ \p ->
    $(testing [|smallestDivisor p|]) (?== p)

ex6_smallestDivisor_comp = do
  k <- (elements . take 10 $ primes)
  p <- (elements . take 20 . drop 10 $ primes)
  let n = k*p
  return $ $(testing [|smallestDivisor n|]) (?== k)

ex7_isPrime =
  forAllBlind (elements [0..max]) $ \n ->
  $(testing [|isPrime n|]) (?== elem n primes')
  where max = 20
        primes' = takeWhile (<=max) primes

ex8_biggestPrimeAtMost =
  forAllBlind (elements [2..max]) $ \n ->
  $(testing [|biggestPrimeAtMost n|]) (?== last (takeWhile (<=n) primes))
  where max = 100
