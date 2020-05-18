{-# LANGUAGE TemplateHaskell #-}

module Set1Test where

import Data.List
import Test.QuickCheck

import Mooc.Th
import Mooc.Test

import Set1

main = score tests

tests = [(1,"onetwo",[ex1_one_defined, ex1_one_type
                     ,ex1_two_defined, ex1_two_type])
        ,(2,"double",[ex2_double])
        ,(3,"quadruple",[ex3_quadruple])
        ,(4,"distance",[property ex4_distance])
        ,(5,"eeny",[property ex5_eeny_even, property ex5_meeny_odd])
        ,(6,"checkPassword",[property ex6_denied, property ex6_accepted])
        ,(7,"postagePrice",[property ex7_corners
                           ,property ex7_cheap
                           ,property ex7_medium
                           ,property ex7_large])
        ,(8,"isZero",[property ex8_isZero_0
                     ,property ex8_isZero_positive
                     ,property ex8_isZero_negative])
        ,(9,"sumTo",[ex9_sumTo])
        ,(10,"power",[ex10_power])
        ,(11,"ilog3",[property ex11_ilog3])]

-- -- -- -- -- --

ex1_one_defined = $(isDefined "one")
ex1_two_defined = $(isDefined "two")
ex1_one_type = $(hasType "one" [t|Int|]) (?==1)
ex1_two_type = $(hasType "two" [t|Int|]) (?==2)

ex2_double = forAll_ $ \x -> $(testing [|double x|]) (?==2*x)

ex3_quadruple = forAll_ $ \x -> $(testing [|quadruple x|]) (?==4*x)

ex4_distance = do
  (deltax,deltay,dist) <- elements ([(3,4,5),(5,12,13),(8,15,17),(7,24,25)] :: [(Double,Double,Double)])
  x1 <- elements [-5..5]
  y1 <- elements [-5..5]
  let x2 = x1+deltax
  let y2 = y1+deltay
  return $ $(testing [|distance x1 y1 x2 y2|]) (dist=~?)

ex5_eeny_even = forAll_ $ \x -> $(testing [|eeny (2*x)|]) (?=="eeny")
ex5_meeny_odd = forAll_ $ \x -> $(testing [|eeny (2*x+1)|]) (?=="meeny")

word = listOf1 (choose ('a','z'))
passwords = ["swordfish","mellon"]

ex6_denied = do
  w <- word `suchThat` (\w -> not (elem w passwords))
  return $ $(testing [|checkPassword w|]) (?=="ACCESS DENIED!")

ex6_accepted = do
  w <- elements passwords
  return $ $(testing [|checkPassword w|]) (?=="You're in.")

ex7_corners =
  $(testing [|postagePrice 500|]) (?==250)
  .&&.
  $(testing [|postagePrice 5000|]) (?==5300)

ex7_cheap =
  forAllBlind (choose (0,500)) $ \w ->
  $(testing [|postagePrice w|]) (?==250)

ex7_medium =
  forAllBlind (choose (501,5000)) $ \w ->
  $(testing [|postagePrice w|]) (?==300+w)

ex7_large =
  forAllBlind (choose (5001,10000)) $ \w ->
  $(testing [|postagePrice w|]) (?==6000)

ex8_isZero_0 = $(testing [|isZero 0|]) (?== True)

ex8_isZero_positive :: Positive Integer -> Property
ex8_isZero_positive (Positive n) = $(testing [|isZero n|]) (?== False)

ex8_isZero_negative :: Positive Integer -> Property
ex8_isZero_negative (Positive n) = $(testing [|isZero n|]) (?== False)

ex9_sumTo =
  forAllBlind (elements [1..100]) $ \n ->
  $(testing [|sumTo n|]) (?==sum [1..n])

ex10_power =
  forAllBlind (elements [1..10]) $ \n ->
  forAllBlind (elements [1..10]) $ \k ->
  $(testing [|power n k|]) (?==n^k)

ex11_ilog3 (Blind (Positive n)) =
  $(testing [|ilog3 n|]) (?== 1 + floor (logBase 3 $ fromIntegral n))
