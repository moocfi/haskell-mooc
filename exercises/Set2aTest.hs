{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Set2aTest where

import Data.Char
import Data.List
import Test.QuickCheck

import Mooc.Th
import Mooc.Test

import Set2a

main = score tests

tests = [(1,"years",[ex1_years])
        ,(2,"takeFinal",[ex2_takeFinal_1, ex2_takeFinal_2])
        ,(3,"updateAt",[ex3_updateAt])
        ,(4,"substring",[ex4_substring])
        ,(5,"isPalindrome",[ex5_isPalindrome])
        ,(6,"palindromify",[ex6_palindromify])
        ,(7,"safeDiv",[ex7_safeDiv_zero, ex7_safeDiv_nonZero])
        ,(8,"greet",[ex8_greet_nothing, ex8_greet_just])
        ,(9,"safeIndex",[ex9_safeIndex_ok, ex9_safeIndex_fail])
        ,(10,"eitherDiv",[ex10_eitherDiv_zero, ex10_eitherDiv_nonZero])
        ,(11,"addEithers",[ex11_addEithers_both, ex11_addEithers_one, ex11_addEithers_neither])
        ]

-- -- -- -- -- --

ex1_years =
  let origLength = length in
    let length = origLength :: [a] -> Int in
      conjoin [$(testing' [|length years|]) (?== 3)
              ,$(testing' [|head years|]) (?== 1982)
              ,$(testing' [|years !! 1|]) (?== 2004)
              ,$(testing' [|last years|]) (?== 2020)]

letter = choose ('a','z')
word = listOf1 letter

ex2_takeFinal_1 = property $ do
  n <- choose (0,20)
  k <- choose (0,n)
  return $ $(testing [|takeFinal k [0..n]|]) (?== [n-k+1..n])

ex2_takeFinal_2 = property $ do
  n <- choose (0,20)
  k <- choose (0,20)
  let inp = reverse [0..n]
  return $ $(testing [|takeFinal k inp|]) (?== reverse [0..min (k-1) n])

ex3_updateAt = property $ do
  ws <- listOf1 word
  i <- choose (0,length ws - 1)
  j <- choose (0,length ws - 1)
  w <- word
  return . $(testing [|updateAt i w ws|]) $ \v ->
    conjoin [counterexample ("index "++show i) $ v!!i ?== w
            ,counterexample ("index "++show j) $ i/=j ==> v!!j ?== ws!!j]

ex4_substring = forAll_ $ \(prefix :: String, target, suffix) ->
  $(testing [|substring (length prefix) (length prefix + length target) (prefix ++ target ++ suffix)|]) (?==target)

ex5_isPalindrome = forAllBlind word $ \w1 ->
  forAllBlind (word `suchThat` ((/=head w1).last) `suchThat` ((>1).length)) $ \w2 ->
  conjoin [$(testing [|isPalindrome (w1 ++ w2)|]) (?==False)
          ,$(testing [|isPalindrome (w1 ++ reverse w1)|]) (?==True)
          ,head w2 /= last w2 ==> $(testing [|isPalindrome (w1 ++ w2 ++ reverse w1)|]) (?==False)
          ,$(testing [|isPalindrome (init w1 ++ reverse w1)|]) (?==True)]


ex6_palindromify = property $ do
  beef <- word
  center <- sublistOf "z"
  bun <- fmap nub word `suchThat` \w -> length w > 1
  let answer = beef ++ center ++ reverse beef
      input = bun ++ answer ++ bun
  return $ $(testing [|palindromify input|]) (?== answer)

ex7_safeDiv_zero = forAll_ $ \x ->
  $(testing [|safeDiv x 0|]) (?== Nothing)

ex7_safeDiv_nonZero = forAll_ $ \(x,y) ->
  (y/=0) ==> $(testing [|safeDiv x y|]) (?== Just (div x y))

ex8_greet_nothing = forAllBlind word $ \first ->
  $(testing [|greet first (Nothing :: Maybe String)|]) (?==("Hello, "++first++"!"))

ex8_greet_just = forAllBlind word $ \first ->
  forAllBlind word $ \last ->
  $(testing [|greet first (Just last)|]) (?==("Hello, "++first++" "++last++"!"))

ex9_safeIndex_ok = property $ do
  ws <- listOf1 word
  i <- choose (0,length ws - 1)
  return $ $(testing [|safeIndex ws i|]) (?==Just (ws!!i))

ex9_safeIndex_fail = property $ do
  is <- listOf1 (choose (10,20::Int))
  i <- choose (0,10)
  let neg = negate i - 1
  let over = length is + i
  return $ conjoin [$(testing [|safeIndex is neg|]) (?==Nothing)
                   ,$(testing [|safeIndex is over|]) (?==Nothing)]

ex10_eitherDiv_zero = forAll_ $ \x ->
  $(testing [|eitherDiv x 0|]) (?== Left (show x++"/0"))

ex10_eitherDiv_nonZero = forAll_ $ \(x,y) ->
  (y/=0) ==> $(testing [|eitherDiv x y|]) (?== Right (div x y))


ex11_addEithers_both = forAll_ $ \(x,y) ->
  $(testing [|addEithers (Right x :: Either String Int) (Right y :: Either String Int)|]) (?== Right (x+y))

ex11_addEithers_one = property $ do
  err <- fmap Left arbitrary
  int <- fmap Right arbitrary
  ~[x,y] <- shuffle [err,int]
  return $ $(testing [|addEithers x y|]) (?==err)

ex11_addEithers_neither = forAll_ $ \(a,b) ->
  $(testing [|addEithers (Left a :: Either String Int) (Left b :: Either String Int)|]) (?==Left a)
