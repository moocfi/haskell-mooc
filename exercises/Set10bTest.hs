{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Set10bTest where

import Mooc.Test
import Mooc.Th

import Control.Exception (evaluate, try, SomeException)
import Data.List
import Test.QuickCheck
import Test.QuickCheck.Monadic


import Set10b

main = score tests

tests = [(1,"|||",[imports, ex1_normal, ex1_undefined])
        ,(2,"boolLength",[imports, ex2_normal, ex2_undefined])
        ,(3,"validate",[imports, ex3_normal, ex3_fail])
        ,(4,"myseq",[imports, ex4_bool, ex4_int, ex4_list_ok, ex4_list_fail])
        ]

-- -- -- -- --

imports = $(importsOnly "Set10b" ["GHC.Num", "GHC.Base", "GHC.Classes", "GHC.Types", "GHC.Err",
                                  "Mooc.Todo", "Mooc.VeryLimitedPrelude"])

ex1_normal = forAll_ $ \(a,b) ->
  $(testing [|a ||| b|]) (?==(a||b))

ex1_undefined =
    counterexample ("undefined ||| True") $ (errorWithoutStackTrace "You forced your left argument!"|||True) == True

ex2_normal = forAll_ $ \bs -> boolLength bs === length bs

ex2_undefined = forAll_ $ \bs ->
  counterexample ("boolLength ("++show bs++"++[undefined,True])") $
    shouldFail (boolLength (bs++[undefined,True]))

ex3_normal = forAll_ $ \(n::Int) ->
  counterexample ("validate even "++show n) $
  validate even n ?== n

ex3_fail = conjoin [counterexample "validate undefined 'c'" $ shouldFail (validate undefined 'c')
                   ,counterexample "validate (\\x -> undefined) \"foo\"" $ shouldFail (validate (\x -> undefined) "foo")]

ex4_bool = $(withInstance "MySeq" "Bool" [|myseq :: Bool -> a -> a|]) $ \myseq ->
  forAll_ $ \(i::Int) ->
  conjoin [$(testing [|myseq True i|]) (?==i)
          ,$(testing [|myseq False i|]) (?==i)
          ,counterexample ("myseq (undefined::Bool) " ++ show i) $ shouldFail (myseq (undefined::Bool) i)]

ex4_int = $(withInstance "MySeq" "Int" [|myseq :: Int -> a -> a|]) $ \myseq ->
  forAll_ $ \(i::Int,b::Bool) ->
  conjoin [$(testing [|myseq i b|]) (?==b)
          ,counterexample ("myseq (undefined::Int) " ++ show b) $ shouldFail (myseq (undefined::Int) b)]

ex4_list_ok = $(withInstance1 "MySeq" "[]" [|myseq :: [Int] -> a -> a|]) $ \myseq ->
  forAll_ $ \(is::[Int]) ->
  forAll_ $ \(i::Int) ->
  forAllBlind (choose ('a','z')) $ \c ->
  $(testing [|myseq is c|]) (?==c)
  .&&.
  conjoin' [counterexample ("myseq (undefined:" ++ show is ++") " ++ show c) $ counterexample "  should not fail" $ (myseq (undefined:is) c ?== c)
           ,counterexample ("myseq (" ++ show is ++ "++[undefined]) " ++ show c) $ counterexample "  should not fail" $ (myseq (is++[undefined]) c ?== c)
           ,counterexample ("myseq (" ++ show i ++ ":undefined) " ++ show c) $ counterexample "  should not fail" $ (myseq (i:undefined) c ?== c)]

ex4_list_fail = $(withInstance1 "MySeq" "[]" [|myseq :: [Int] -> a -> a|]) $ \myseq ->
  forAllBlind (choose ('a','z')) $ \c ->
  counterexample ("myseq (undefined::[Int]) "++show c) $ shouldFail (myseq (undefined::[Int]) c)
