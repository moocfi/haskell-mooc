{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Set3bTest where

import Data.List
import Test.QuickCheck

import Mooc.Th
import Mooc.Test

import Set3b

main = score tests

tests = [(1,"buildList",[imports, ex1_buildList])
        ,(2,"sums",[imports, ex2_sums])
        ,(3,"mylast",[imports, ex3_mylast_nonempty, ex3_mylast_empty])
        ,(4,"indexDefault",[imports, ex4_indexDefault_ok, ex4_indexDefault_fail])
        ,(5,"sorted",[imports, ex5_sorted_empty, ex5_sorted])
        ,(6,"sumsOf",[imports, ex6_sumsOf])
        ,(7,"merge",[imports, ex7_merge])
        ,(8,"mymaximum",[imports, ex8_mymaximum_max, ex8_mymaximum_min, ex8_mymaximum_empty])
        ,(9,"map2",[imports, ex9_map2_1, ex9_map2_2])
        ,(10,"maybeMap",[imports, ex10_maybeMap_1, ex10_maybeMap_2])
        ]

-- -- -- -- -- --

imports = $(importsOnly "Set3b" ["GHC.Num", "GHC.Real", "GHC.Base", "GHC.Classes", "GHC.Types",
                                 "Mooc.Todo", "Mooc.LimitedPrelude"])

ex1_buildList = property $ do
  start <- choose (0,20)
  count <- choose (0,10)
  end <- choose (30,40)
  return $ $(testing [|buildList start count end|]) (?==(replicate count start ++ [end]))

ex2_sums = property $ do
  i <- choose (1,30)
  return $ $(testing [|sums i|]) (?==scanl1 (+) [1..i])


ex3_mylast_nonempty = forAll_ $ \(NonEmpty xs::NonEmptyList Integer) ->
  $(testing [|mylast 0 xs|]) (?== last xs)

ex3_mylast_empty = forAll_ $ \(b::Bool) ->
  $(testing [|mylast b ([]::[Bool])|]) (?== b)

ex4_indexDefault_ok = property $ do
  ws <- listOf1 arbitrary :: Gen [Int]
  i <- choose (0,length ws - 1)
  def <- arbitrary
  return $ $(testing [|indexDefault ws i def|]) (?==ws!!i)

ex4_indexDefault_fail = property $ do
  is <- listOf1 arbitrary :: Gen [Int]
  i <- choose (0,10)
  def <- arbitrary
  let neg = negate i - 1
  let over = length is + i
  return $ conjoin [$(testing [|indexDefault is neg def|]) (?==def)
                   ,$(testing [|indexDefault is over def|]) (?==def)]

ex5_sorted_empty =
  $(testing [|sorted ([]::[Int])|]) (?== True)

ex5_sorted = forAll_ $ \xs ->
  let s = sort xs
  in conjoin [$(testing [|sorted xs|]) (?== (s == xs))
             ,$(testing [|sorted s|]) (?== True)]


ex6_sumsOf = forAll_ $ \xs -> $(testing [|sumsOf xs|]) (?== scanl1 (+) xs)

ex7_merge = forAll_ $ \(xs,ys) ->
  $(testing [|merge (sort xs) (sort ys)|]) (?== sort (xs ++ ys))

ex8_mymaximum_max = forAll_ $ \(NonEmpty xs :: NonEmptyList Int) ->
  counterexample ("mymaximum (>) 0 " ++ show xs) $
    mymaximum (>) 0 xs ?== max 0 (maximum xs)

ex8_mymaximum_min = forAll_ $ \(NonEmpty xs :: NonEmptyList Int) ->
  counterexample ("mymaximum (<) 10000 " ++ show xs) $
    mymaximum (<) 10000 xs ?== min 10000 (minimum xs)

ex8_mymaximum_empty = property $ do
  i <- choose (0,10) :: Gen Int
  return $ counterexample ("mymaximum (<) "++show i++" []") $ mymaximum (<) i [] ?== i

ex9_map2_1 = forAll_ $ \(i::[Int],j::[Bool]) ->
  counterexample ("map2 const "++show i++" "++show j) $
    map2 const i j ?== take (length j) i

ex9_map2_2 = forAll_ $ \(is::[Int],j::Int) ->
  let js = replicate (length is) j
  in counterexample ("map2 (+) "++show is++" "++show js) $
     map2 (+) is js ?== map (+j) is

ex10_maybeMap_1 = forAll_ $ \bs ->
  counterexample ("let f True = Just True; f False = Nothing in maybeMap f "++show bs) $
  maybeMap f bs ?== filter id bs
  where f True = Just True
        f False = Nothing

ex10_maybeMap_2 = forAll_ $ \(is :: [Int]) ->
  counterexample ("let f x = if x>0 then Just (2*x) else Nothing\
                 \in maybeMap f "++show is) $
  maybeMap f is ?== map (2*) (filter (>0) is)
  where f x = if x>0 then Just (2*x) else Nothing
