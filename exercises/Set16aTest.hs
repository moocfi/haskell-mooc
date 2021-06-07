{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Set16aTest where

import Mooc.Test
import Mooc.Th

import Control.Monad
import Data.List
import Test.QuickCheck
import Test.QuickCheck.Monadic hiding (pick)

import Set16a

main = score tests

tests = [(1,"isSorted",[ex1_1, ex1_2])
        ,(2,"sumIsLength",[ex2])
        ,(3,"inputInOutput",[ex3])
        ,(4,"outputInInput",[ex4])
        ,(5,"frequenciesProp",[ex5_1, ex5_2, ex5_3, ex5_4])
        ,(6,"genList",[ex6_1,ex6_2])
        ,(7,"Arbitrary Expression",[ex7_arg_1, ex7_arg_2, ex7_exp])]

-- -- -- -- --

qPass p = monadicIO $ do
  res <- run $ quickCheckWithResult quietArgs p
  stop_ $
    counterexample "  Should pass" $
    counterexample "quickCheck output:" $
    counterexample (output res) $
    isSuccess res

qFail p = monadicIO $ do
  res <- run $ quickCheckWithResult quietArgs p
  stop_ $
    counterexample "  Should fail" $
    counterexample "quickCheck output:" $
    counterexample (output res) $
    case res of Failure{} -> True
                _ -> False

letter = choose ('a','z')

ex1_1 = conjoin [$(testing [|isSorted [1,2,3]|]) qPass
                ,$(testing [|isSorted [1,3,2]|]) qFail
                ,$(testing [|isSorted ([]::[Int])|]) qPass]

ex1_2 = forAll (listOf1 (choose (-2,4::Integer))) $ \deltas ->
  let inp = scanl (+) 3 deltas
  in $(testing [|isSorted inp|]) (if all (>=0) deltas then qPass else qFail)

ex2 = forAllBlind (listOf (choose (0,3))) $ \ns ->
  forAllBlind (vectorOf (length ns) letter) $ \cs ->
  forAllBlind (vectorOf (sum ns) letter) $ \inp ->
  let out = zip cs ns
  in conjoin [$(testing [|sumIsLength inp out|]) qPass
             ,not (null inp) ==> forAllBlind (choose (1,length inp)) (\k -> $(testing [|sumIsLength (drop k inp) out|]) qFail)
             ,forAllBlind (choose (1,length out)) (\k -> sum (take k ns) /= 0 ==> $(testing [|sumIsLength inp (drop k out)|]) qFail)]

ex3 = forAllBlind (listOf1 letter) $ \inp ->
  forAllBlind (vectorOf (length inp) (choose (0,100))) $ \ns ->
  forAllBlind (shuffle (zip (nub inp) ns)) $ \out ->
  forAllBlind (sublistOf inp `suchThat` (not.null)) $ \inp' ->
  forAllBlind (sublistOf out) $ \out' ->
  conjoin [$(testing [|inputInOutput inp' out|]) qPass
          ,out' /= out ==> $(testing [|inputInOutput inp out'|]) qFail]

outToIn = concatMap (\(c,i) -> replicate i c)

chg :: (a,Int) -> Gen (a,Int)
chg (c,i) = do j <- choose (1,5) `suchThat` (/=i)
               return (c,j)

changeCount :: [(a,Int)] -> Gen [(a,Int)]
changeCount [p] = fmap (:[]) $ chg p
changeCount (p:ps) = oneof [fmap (p:) $ changeCount ps
                           ,fmap (:ps) $ chg p]

ex4 = forAllBlind (choose (1,8)) $ \n ->
  forAllBlind (vectorOf n letter) $ \cs ->
  forAllBlind (vectorOf (length cs) (choose (1,5))) $ \ns ->
  forAllBlind (shuffle (zip (nub cs) ns)) $ \out ->
  forAllBlind (shuffle (outToIn out)) $ \inp ->
  forAllBlind (sublistOf out `suchThat` (not.null)) $ \out' ->
  forAllBlind (sublistOf inp) $ \inp' ->
  conjoin [$(testing [|outputInInput inp out'|]) qPass
          ,inp' /= inp ==> $(testing [|outputInInput inp' out|]) qFail
          ,forAllBlind (changeCount out) $ \out2 ->
              $(testing [|outputInInput inp out2|]) qFail]

ex5_1 = $(testing' [|frequenciesProp freq1|]) qFail
ex5_2 = $(testing' [|frequenciesProp freq2|]) qFail
ex5_3 = $(testing' [|frequenciesProp freq3|]) qFail
ex5_4 = $(testing' [|frequenciesProp frequencies|]) qPass

testGenMany :: (Show t) => Gen t -> Int -> ([t] -> Property) -> Property
testGenMany gen i k = monadicIO $ do
  xs <- run $ replicateM i (generate gen)
  monitor (counterexample ("Generated values: " ++ show xs))
  stop_ $ k xs

testGen :: (Show t) => Gen t -> (t -> Property) -> Property
testGen gen k = monadicIO $ do
  x <- run $ generate gen
  monitor (counterexample ("Generated value: " ++ show x))
  stop_ $ k x

ex6_1 = testGen genList $ \l ->
  conjoin [counterexample "  length should be 3-5" $ length l >= 3 && length l <= 5
          ,counterexample "  all elements should be 0-10" $ all (\x -> x>=0 && x<=10) l
          ,counterexample "  list should be sorted" $ sort l == l]

ex6_2 = once $ testGenMany genList 30 $ \ls ->
  conjoin [counterexample "  should have lists of all lengths" $ nub (sort (map length ls)) ?== [3,4,5]
          ,counterexample "  should at least 7 different ints" $ length (nub (sort (concat ls))) >= 7
          ,counterexample "  should have a list with differing elements" $ any ((>1).length.nub) ls]

ex7_arg_1 = counterexample "arbitrary :: Gen Arg" $
  testGen (arbitrary :: Gen Arg) $ \a ->
  case a of Number i -> counterexample "  number should be in the range 0-10" $ i>=0 && i<=10
            Variable c -> counterexample "  variable should be one of abcxyz" $ elem c "abcxyz"

ex7_arg_2 = once $ counterexample "arbitrary :: Gen Arg" $
  testGenMany (arbitrary :: Gen Arg) 20 $ \as ->
  counterexample "  should generate both Number and Variable values" $ any isVariable as && any (not.isVariable) as
  where isVariable (Variable _) = True
        isVariable _ = False

ex7_exp = once $ counterexample "arbitrary :: Gen Expression" $
  testGenMany (arbitrary :: Gen Expression) 64 $ \es ->
  counterexample "  should generate all cases" $ nub (sort (map cla es)) ?==
  ["MinusNumberNumber","MinusNumberVariable","MinusVariableNumber","MinusVariableVariable"
  ,"PlusNumberNumber","PlusNumberVariable","PlusVariableNumber","PlusVariableVariable"]
  where cl (Variable _) = "Variable"
        cl (Number _) = "Number"
        cla (Plus x y) = "Plus" ++ cl x ++ cl y
        cla (Minus x y) = "Minus" ++ cl x ++ cl y
