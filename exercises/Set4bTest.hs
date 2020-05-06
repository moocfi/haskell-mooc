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
        ,(3,"myHead",[ex3_myHead_empty, ex3_myHead_full])
        ,(4,"myLast",[ex4_myLast_empty, ex4_myLast_full])
        ,(5,"sumAndLength",[ex5_sumAndLength])
        ,(6,"myConcat",[ex6_myConcat])]

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

ex3_myHead_empty =
  $(testing' [|foldr headHelper Nothing []|]) (?==(Nothing::Maybe Bool))

ex3_myHead_full =
  forAll_ $ \(NonEmpty xs::NonEmptyList Int) ->
  counterexample ("xs = "++show xs) $
  $(testing' [|foldr headHelper Nothing xs|]) (?==Just (head xs))

ex4_myLast_empty =
  $(testing' [|foldr lastHelper Nothing []|]) (?==(Nothing::Maybe Bool))

ex4_myLast_full =
  forAll_ $ \(NonEmpty xs::NonEmptyList Int) ->
  counterexample ("xs = "++show xs) $
  $(testing' [|foldr lastHelper Nothing xs|]) (?==Just (last xs))

niceDouble = fmap ((/2).fromIntegral) (arbitrary :: Gen Int)

ex5_sumAndLength =
  forAllBlind (listOf niceDouble) $ \(ds::[Double]) ->
  counterexample ("ds = "++show ds) $
  $(testing' [|foldr slHelper slStart ds|]) (?==(sum ds, length ds))

ex6_myConcat =
  forAll_ $ \(xs :: [[Int]]) ->
  counterexample ("xs = "++show xs) $
  $(testing' [|foldr concatHelper concatStart xs|]) (?==concat xs)
