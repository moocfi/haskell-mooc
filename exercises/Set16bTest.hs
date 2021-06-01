{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Set16bTest where

import Mooc.Test
import Mooc.Th

import Examples.Phantom

import Control.Monad
import Data.Char
import Data.List
import Test.QuickCheck
import Test.QuickCheck.Monadic hiding (pick)

import Set16b

ex1_t = $(hasType' "pounds" "Money GBP") (const True)

ex1_v = $(testing' [|pounds|]) (?==Money 3.0)

niceFloat = (/2) <$> fromIntegral <$> choose (1::Int,10)

ex2_v = forAllBlind niceFloat $ \r1 ->
  forAllBlind niceFloat $ \r2 ->
  $(testing [|composeRates (Rate r1) (Rate r2)|]) (?==Rate (r1*r2))

eurToChf = composeRates eurToUsd usdToChf

composeTest1 x = composeRates usdToChf (composeRates x eurToUsd)

bob = toFirst "bob"

smith = toLast "smith"

capitalBob = capitalize (toFirst "bob")

capitalSmith = capitalize (toLast "smith")

bobSmith = toFull (toFirst "bob") (toLast "smith")

aSmith x = toFull x (toLast "smith")

aBob x = toFull (toFirst "bob") x

$(return []) -- hack to push above definitions into reify scope

ex2_t1 =
  counterexample "eurToChf = composeRates eurToUsd usdToChf" $
  $(hasType' "eurToChf" "Rate EUR CHF") (const True)

ex2_t2 =
  counterexample "composeTest1 x = composeRates usdToChf (composeRates x eurToUsd)" $
  $(hasType' "composeTest1" "Rate CHF EUR -> Rate USD USD") (const True)

letter = choose ('a','z')

word = listOf1 letter

ex3_v = forAll word $ \w ->
  conjoin [counterexample ("fromName (toFirst "++show w++")") $
           fromName (toFirst w) ?== w
          ,counterexample ("fromName (toLast "++show w++")") $
           fromName (toLast w) ?== w]

ex3_t1 =
  counterexample "bob = toFirst \"bob\"" $
  $(hasType' "bob" "Name First") (const True)

ex3_t2 =
  counterexample "smith = toLast \"smith\"" $
  $(hasType' "smith" "Name Last") (const True)

ex4_v1 = forAllBlind word $ \w ->
  counterexample ("fromName (capitalize (toFirst "++show w++"))") $
  fromName (capitalize (toFirst w)) ?== zipWith id (toUpper:repeat id) w

ex4_t1 =
  conjoin [counterexample "capitalBob = capitalize (toFirst \"bob\")" $
           $(hasType' "capitalBob" "Name First") (const True)
          ,counterexample "capitalSmith = capitalize (toLast \"smith\")" $
           $(hasType' "capitalSmith" "Name Last") (const True)]

ex4_v2 = forAllBlind word $ \f ->
  forAllBlind word $ \l ->
  counterexample ("fromName (toFull (toFirst "++show f++") (toLast "++show l++"))") $
  fromName (toFull (toFirst f) (toLast l)) ?== f ++ " " ++ l

ex4_t2 =
  conjoin [counterexample "bobSmith = toFull (toFirst \"bob\") (toLast \"smith\")" $
           $(hasType' "bobSmith" "Name Full") (const True)
          ,counterexample "aSmith x = toFull x (toLast \"smith\")" $
           $(hasType' "aSmith" "Name First -> Name Full") (const True)
          ,counterexample "aBob x = toFull (toFirst \"bob\") x" $
           $(hasType' "aBob" "Name Last -> Name Full") (const True)]

ex5_e = counterexample "render (Money 1.0 :: Money EUR)" $
  $(withInstanceSilent "Render" "EUR" [|render|] " Type error!") $ \render ->
  render (Money 1.0 :: Money EUR) ?=="1.0e"

ex5_u = counterexample "render (Money 1.0 :: Money USD)" $
  $(withInstanceSilent "Render" "USD" [|render|] " Type error!") $ \render ->
  render (Money 1.0 :: Money USD) ?=="$1.0"

ex5_c = counterexample "render (Money 1.0 :: Money CHF)" $
  $(withInstanceSilent "Render" "CHF" [|render|] " Type error!") $ \render ->
  render (Money 1.0 :: Money CHF) ?=="1.0chf"

-- -- -- -- --

tests = [(1,"pounds",[ex1_t,ex1_v])
        ,(2,"composeRates",[ex2_v, ex2_t1, ex2_t2])
        ,(3,"firstlastfull1",[ex3_v, ex3_t1, ex3_t2])
        ,(4,"firstlastfull2",[ex4_v1, ex4_t1, ex4_v2, ex4_t2])
        ,(5,"Render currency",[ex5_e, ex5_u, ex5_c])]

main = score tests
