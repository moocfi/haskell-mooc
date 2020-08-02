{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Set4bTest where

import Data.Char
import Data.List
import Test.QuickCheck

import Mooc.Th
import Mooc.Test

import Set4b

main = score tests

tests = [(1,"countNothings",[ex1_countNothings])
        ,(2,"myMaximum",[ex2_myMaximum])
        ,(3,"sumAndLength",[ex3_sumAndLength])
        ,(4,"myConcat",[ex4_myConcat])
        ,(5,"largest",[ex5_largest])
        ,(6,"myHead",[ex6_myHead_empty, ex6_myHead_full])
        ,(7,"myLast",[ex7_myLast_empty, ex7_myLast_full])]

ex1_countNothings = property $ do
  justs <- fmap (map Just) $ listOf (choose (0::Int,10))
  nothings <- listOf (return Nothing)
  input <- shuffle (justs++nothings)
  return $
    counterexample ("input = "++show input) $
    $(testing' [|foldr countHelper 0 input|]) (?==length nothings)

ex2_myMaximum = property $ do
  x <- choose (-100::Int,100)
  xs <- listOf (choose (-100,100::Int))
  return $
    counterexample ("x = "++show x) $
    counterexample ("xs = "++show xs) $
    $(testing' [|foldr maxHelper x xs|]) (?==maximum (x:xs))

niceDouble = fmap ((/2).fromIntegral) (arbitrary :: Gen Int)

ex3_sumAndLength =
  forAllBlind (listOf niceDouble) $ \(ds::[Double]) ->
  counterexample ("ds = "++show ds) $
  $(testing' [|foldr slHelper slStart ds|]) (?==(sum ds, length ds))

ex4_myConcat =
  forAll_ $ \(xs :: [[Int]]) ->
  counterexample ("xs = "++show xs) $
  $(testing' [|foldr concatHelper concatStart xs|]) (?==concat xs)

ex5_largest =
  forAll_ $ \(NonEmpty (xs :: [Int])) ->
  counterexample ("xs = "++show xs) $
  $(testing' [|foldr largestHelper [] xs|]) (?==filter (==maximum xs) xs)


ex6_myHead_empty =
  $(testing' [|foldr headHelper Nothing []|]) (?==(Nothing::Maybe Bool))

ex6_myHead_full =
  forAll_ $ \(NonEmpty xs::NonEmptyList Int) ->
  counterexample ("xs = "++show xs) $
  $(testing' [|foldr headHelper Nothing xs|]) (?==Just (head xs))

ex7_myLast_empty =
  $(testing' [|foldr lastHelper Nothing []|]) (?==(Nothing::Maybe Bool))

ex7_myLast_full =
  forAll_ $ \(NonEmpty xs::NonEmptyList Int) ->
  counterexample ("xs = "++show xs) $
  $(testing' [|foldr lastHelper Nothing xs|]) (?==Just (last xs))
