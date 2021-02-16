{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, StandaloneDeriving #-}

module Set13aTest where

import Mooc.Test
import Mooc.Th

import Control.Monad.Trans.State
import Data.Char
import Data.List
import qualified Data.Map as Map
import Test.QuickCheck

import Examples.Bank
import Set13a

main = score tests

tests = [(1,"readNames",[ex1_ok, ex1_fail])
        ,(2,"winnerMaybe",[ex2_small, ex2_big, ex2_fail])
        ,(3,"selectSum",[ex3_ok, ex3_fail])
        ,(4,"countAndLog",[ex4])
        ,(5,"balance",[ex5_balance_examples, ex5_balance_ok, ex5_balance_nok])
        ,(6,"rob",[ex6_rob_examples, ex6_rob])
        ,(7,"State update",[ex7])
        ,(8,"parensMatch",[ex8_paren, ex8_parensMatch])
        ,(9,"State count",[ex9])
        ,(10,"State occurrences",[ex10_1, ex10_2])
        ]

-- -- -- -- -- -- --

word = do fst <- choose ('A','Z')
          rest <- listOf (choose ('a','z'))
          return $ fst:rest

bad = do a <- choose ('A','Z')
         b <- word
         c <- elements "0123456789"
         d <- word
         return $ a:b++c:d

ex1_ok =
  forAllBlind word $ \for ->
  forAllBlind word $ \sur ->
  let str = for++" "++sur
  in $(testing [|readNames str|])
     (?==Just (for,sur))

m_ex1_fail s =
  $(testing [|readNames s|]) (?== Nothing)

ex1_fail =
  forAllBlind word $ \for ->
  forAllBlind word $ \sur ->
  forAllBlind bad $ \b ->
  conjoin [m_ex1_fail (for ++ sur),
           m_ex1_fail (map toLower for ++ " " ++ sur),
           m_ex1_fail (for ++ " " ++ map toLower sur),
           m_ex1_fail (for ++ b ++ " " ++ sur),
           m_ex1_fail (for ++ " " ++ sur ++ b)]

ex2_small =
  forAllBlind word $ \p1 ->
  forAllBlind (word `suchThat` (/=p1)) $ \p2 ->
  forAllBlind (choose (1,20)) $ \s1 ->
  forAllBlind (choose (0,s1)) $ \s2 ->
  forAllBlind (shuffle [(p1,s1),(p2,s2)]) $ \inp ->
  conjoin [$(testing [|winner inp p1 p2|]) (?==Just p1)
          ,s2 < s1 ==> $(testing [|winner inp p2 p1|]) (?==Just p1)]

ex2_big =
  forAllBlind word $ \w ->
  forAllBlind (choose (10,100)) $ \s ->
  forAllBlind (nub <$> listOf1 (word`suchThat`(/=w))) $ \names ->
  forAllBlind (vectorOf (length names) (choose (0,s-1))) $ \scores ->
  forAllBlind (elements names) $ \other ->
  forAllBlind (shuffle ((w,s):zip names scores)) $ \inp ->
  conjoin [$(testing [|winner inp other w|]) (?==Just w)
          ,$(testing [|winner inp w other|]) (?==Just w)]

ex2_fail =
  forAllBlind (nub <$> listOf1 word) $ \names ->
  forAllBlind (vectorOf (length names) (choose (0,100))) $ \scores ->
  forAllBlind (elements names) $ \ok ->
  forAllBlind (word `suchThat` (not.flip elem names)) $ \nok1 ->
  forAllBlind (word `suchThat` (not.flip elem names)) $ \nok2 ->
  let inp = zip names scores
  in conjoin $[$(testing [|winner inp ok nok1|]) (?==Nothing)
              ,$(testing [|winner inp nok1 ok|]) (?==Nothing)
              ,$(testing [|winner inp nok1 nok2|]) (?==Nothing)]

ex3_ok = forAll_ $ \(as::[Int]) ->
  forAllBlind (shuffle (zip [0..] as)) $ \pairs ->
  forAllShrink_ (sublistOf pairs) $ \selected ->
  $(testing [|selectSum as (map fst selected)|]) (?==Just (sum $ map snd selected))

ex3_fail = property $ do
  as <- arbitrary :: Gen [Int]
  is <- listOf (choose (0,length as - 1))
  b <- elements [-1,length as]
  inp <- shuffle (b:is)
  return $ $(testing [|selectSum as inp|]) (?==Nothing)

ex4 = property $ do
  i <- choose (0::Int,10)
  is <- listOf (choose (0,10) `suchThat` (/=i))
  n <- choose (0,5)
  input <- shuffle (replicate n i ++ is)
  return $ counterexample ("countAndLog (=="++show i++") "++show input) $
    countAndLog (==i) input ?== Logger (replicate n (show i)) n

ex5_balance_examples =
  conjoin [$(testing' [|runBankOp (balance "harry") exampleBank|]) (?==(10,exampleBank))
          ,$(testing' [|runBankOp (balance "sean") exampleBank|]) (?==(0,exampleBank))]

bank = do
  names <- nub <$> listOf1 word
  bals <- vectorOf (length names) (choose (0,100))
  return $ Map.fromList (zip names bals)

ex5_balance_ok = forAllBlind bank $ \b ->
  forAllBlind (elements (Map.keys b)) $ \account ->
  let bank = Bank b
  in $(testing' [|runBankOp (balance account) bank|]) $ \out ->
     counterexample (" with bank = " ++ show bank++"\n      account = "++show account) $
     out?==(b Map.! account, bank)

ex5_balance_nok = forAllBlind bank $ \b ->
  forAllBlind (word `suchThat` (not.flip elem (Map.keys b))) $ \account ->
  let bank = Bank b
  in $(testing' [|runBankOp (balance account) bank|]) $ \out ->
    counterexample (" with bank = " ++ show bank++"\n      account = "++show account) $
    out?==(0,bank)

ex6_rob_examples =
  conjoin [$(testing' [|runBankOp (rob "cedric" "ginny") exampleBank|])
           (?==((),Bank (Map.fromList [("cedric",0),("ginny",8),("harry",10)])))
          ,$(testing' [|runBankOp (rob "sean" "ginny") exampleBank|])
           (?==((),Bank (Map.fromList [("cedric",7),("ginny",1),("harry",10)])))]

ex6_rob = property $ do
  from <- word
  to <- word `suchThat` (/=from)
  others <- listOf (word `suchThat` (not.flip elem [from,to]))
  balf <- choose (0,100)
  balt <- choose (0,100)
  bals <- vectorOf (length others) (choose (0,100))
  let run from to bank exp = $(testing' [|runBankOp (rob from to) bank|]) $ \out ->
        counterexample (" with bank = " ++ show bank++"\n      from = "++show from++"\n      to = "++show to) $
        out ?== ((),exp)
  return $ conjoin [run from to
                    (Bank . Map.fromList $ (from,balf):(to,balt):zip others bals)
                    (Bank . Map.fromList $ (from,0):(to,balf+balt):zip others bals)
                   ,run from to
                    (Bank . Map.fromList $ (to,balt):zip others bals)
                    (Bank . Map.fromList $ (to,balt):zip others bals)]

ex7 = forAll_ $ \i ->
  counterexample ("runState update "++show i) $
  runState update i ?== ((),2*i+1)

ex8_paren =
  forAll_ $ \(NonNegative i) ->
  conjoin [counterexample ("runState (paren '(') "++show i) $
           runState (paren '(') i ?== ((),i+1)
          ,counterexample ("runState (paren ')') "++show i) $
           runState (paren ')') i ?== ((),i-1)
          ,$(testing' [|runState (paren '(') (-1)|]) (?==((),-1))
          ,$(testing' [|runState (paren ')') (-1)|]) (?==((),-1))]

genMatch = frequency [(3,return "()"),
                      (1,(\x -> "("++x++")") <$> genMatch),
                      (1,(++) <$> genMatch <*> genMatch)]

flop ')' = '('
flop '(' = ')'

breakMatch s = do i <- choose (0,length s - 1)
                  let (pre,x:suf) = splitAt i s
                  elements [pre++suf, pre++flop x:suf]

filterMatch s = do x <- elements "()"
                   n <- choose (1,length s)
                   return $ s \\ replicate n x

ex8_parensMatch =
  forAllBlind genMatch $ \match ->
  conjoin [$(testing [|parensMatch match|]) (?==True)
          ,forAllBlind (breakMatch match) $ \broken -> $(testing [|parensMatch broken|]) (?==False)
          ,forAllBlind (filterMatch match) $ \broken -> $(testing [|parensMatch broken|]) (?==False)]

ex9 = property $ do
  is <- fmap nub $ listOf1 (choose ('a','z') :: Gen Char)
  fs <- vectorOf (length is) (choose (1,2048))
  let assocs = zip is fs
  x <- elements is
  y <- choose  ('0','z') `suchThat` \y -> not (elem y is)
  let Just cx = lookup x assocs
      s x = counterexample ("runState (count "++show x++") "++show assocs) . counterexample " Final state"
  return $ conjoin [s y $
                    hasElements ((y,1):assocs) (execState (count y) assocs)
                   ,s x $
                    hasElements ((x,cx+1):delete (x,cx) assocs) (execState (count x) assocs)]

ex10_1 =
  forAllShrink_ (nub <$> listOf (choose ('a','z'))) $ \cs ->
  forAllBlind (vectorOf (length cs) (choose (1,3))) $ \ns ->
  let expanded = concat (zipWith replicate ns cs)
  in forAllBlind (shuffle expanded) $ \input ->
    counterexample ("runState (occurrences "++show input++") []") $
    let out@(x,s) = runState (occurrences input) []
    in counterexample ("  Was: " ++ show out) $
       conjoin [counterexample "Produced value" $ x ?== length cs
               ,counterexample "Final state" $ hasElements (zip cs ns) s]

ex10_2 =
  forAllShrink_ (nub <$> listOf (choose (10,100))) $ \xs ->
  forAllBlind (vectorOf (length xs) (choose (1,3))) $ \ns ->
  forAllBlind (choose (0::Int,9)) $ \x ->
  forAllBlind (choose (1,20)) $ \n0 ->
  forAllBlind (choose (1,20)) $ \n ->
  let state = zip xs ns++[(x,n0)]
      input = replicate n x
  in counterexample ("runState (occurrences "++show input++") "++show state) $
     let out@(ret,s) = runState (occurrences input) state
     in counterexample ("  Was: " ++ show out) $
        conjoin [counterexample "Produced value" $ ret ?== length xs + 1
                ,counterexample "Final state" $ hasElements ((x,n+n0):zip xs ns) s]
