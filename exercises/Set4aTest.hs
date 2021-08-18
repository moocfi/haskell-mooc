{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Set4aTest where

import Control.Monad
import Data.Array
import Data.List
import qualified Data.Map as Map
import Test.QuickCheck

import Mooc.Th
import Mooc.Test

import Set4a

main = score tests

tests = [(1,"allEqual",[ex1, ex1_one])
        ,(2,"distinct",[ex2_int, ex2_char])
        ,(3,"middle",[ex3_char, ex3_int])
        ,(4,"rangeOf",[ex4_int, ex4_integer, ex4_double])
        ,(5,"longest",[ex5_int, ex5_char])
        ,(6,"incrementKey",[ex6_bool_float, ex6_char_int])
        ,(7,"average",[ex7])
        ,(8,"winner",[ex8_winner, ex8_tie])
        ,(9,"freqs",[ex9_bool, ex9_int])
        ,(10,"transfer",[ex10_success, ex10_missing, ex10_notEnough])
        ,(11,"swap",[ex11_swap_small, ex11_swap_large])
        ,(12,"maxIndex",[ex12_bool, ex12_int])]

-- -- -- -- -- -- -- --

m_ex1_true example = forAll_ $ \(cnt,v) ->
  $(testing [|allEqual (replicate cnt (v `asTypeOf` example))|]) (?==True)
m_ex1_false example = forAll_ $ \(v,NonEmpty vs) ->
  not (elem (v `asTypeOf` example) vs) ==> $(testing [|allEqual (v:vs)|]) (?==False)

ex1 = conjoin [m_ex1_true True
              ,m_ex1_true 'a'
              ,m_ex1_false 'a'
              ,m_ex1_true (1::Int)
              ,m_ex1_false (1::Int)]

ex1_one = forAll_ $ \(Positive cnt,x::Int,y) ->
  forAllBlind (shuffle (y:replicate cnt x)) $ \xs ->
  x /= y ==>
  $(testing [|allEqual xs|]) (?==False)

m_ex2 domain = forAllBlind (sublistOf domain) $ \xs ->
  conjoin [forAllBlind (shuffle xs) $ \xs' -> $(testing [|distinct xs'|]) (?==True)
          ,not (null xs) ==> forAllBlind (shuffle (head xs:xs)) $ \xs' -> $(testing [|distinct xs'|]) (?==False)]

ex2_int = m_ex2 [1::Int,2,3,4]

ex2_char = m_ex2 ['a'..'z']



m_ex3 c =
  let d = succ c
      e = succ d
  in forAllBlind (shuffle [c,d,e]) $ \[x,y,z] ->
    $(testing [|middle x y z|]) (?==d)

ex3_char = forAllBlind (choose ('a','y')) m_ex3
ex3_int = forAllBlind (choose (0::Int,20)) m_ex3

genRangeOf delta = do
  base <- arbitrary
  vs <- listOf1 (choose (base, base+delta))
  shuffle (base:base+delta:vs)

m_ex4 delta = forAllBlind (genRangeOf delta) $ \vs ->
  $(testing [|rangeOf vs|]) (?==delta)

ex4_int = forAllBlind (choose (1::Int,10)) m_ex4
ex4_integer = forAllBlind (choose (1::Integer,10)) m_ex4
ex4_double = property $ do
  delta <- elements [1::Int,3,5,7,9]
  vs <- genRangeOf delta
  return $ $(testing [|rangeOf (map conv vs)|]) (?~=conv delta)
    where conv x = fromIntegral x / 2

ex5_int =
  forAllBlind (choose (2,10)) $ \l ->
  forAllBlind (vectorOf l (arbitrary :: Gen Int)) $ \ans ->
  forAllBlind (listOf (choose (1,l - 1) >>= vector)) $ \rest ->
  forAllBlind (shuffle (ans:rest)) $ \input ->
  $(testing [|longest input|]) (?==ans)

ex5_char = property $ do
  c <- choose ('a','k')
  d <- choose ('a','z') `suchThat` (>c)
  len <- choose (1,3)
  crest <- vectorOf len (choose ('a','z'))
  drest <- vectorOf len (choose ('a','z'))
  wrong <- vectorOf len (choose ('a','z'))
  input <- shuffle [c:crest, d:drest, wrong]
  return $ $(testing [|longest input|]) (?==(c:crest))

ex6_bool_float =
    property $ do
      c <- choose (True,False)
      let f i = fromIntegral i + 0.5
      i <- f <$> choose (0,10::Int)
      j <- f <$> choose (0,10::Int)
      let inp = [(c,i),(c,j)]
      return $ $(testing [|incrementKey c inp|]) (?==[(c,i+1),(c,j+1)])

ex6_char_int =
    property $ do
      is <- vectorOf 5 $ choose (0,20::Int)
      ks <- vectorOf 5 $ choose ('a','g')
      c <- choose ('a','h')
      let inp = zip ks is
          out = zipWith (\k i -> if k==c then (k,succ i) else (k,i)) ks is
      return $ $(testing [|incrementKey c inp|]) (?== out)



ex7 = conjoin [forAll_ $ \(Small i) -> $(testing [|average [fromIntegral i :: Float]|]) (?==fromIntegral i)
              ,forAllBlind (choose (1,10)) $ \n -> forAll_ $ \(i::Rational) ->
                  $(testing [|average (replicate n i)|]) (?==i)
              ,forAll_ $ \(Small i) -> let f = fromIntegral i in $(testing [|average [f-1,f,f+1]|]) (?==f)
              ,forAll_ $ \(Small i,Small j) ->
                  let x = fromIntegral i :: Double
                      y = fromIntegral j :: Double
                  in $(testing [|average (replicate 10 x ++ replicate 10 y)|]) (?~=(x+y)/2)]

ex8_winner = property $
  do names <- shuffle ["Mike","Bob","Lisa","Jane"]
     let (win:lose:third:_) = names
     loseScore <- choose (0,10000)
     diff <- choose (1,100)
     thirdScore <- choose (0,20000)
     let inp = Map.fromList [(win,loseScore+diff),(lose,loseScore),(third,thirdScore)]
     return $ conjoin [$(testing [|winner inp win lose|]) (?==win)
                      ,$(testing [|winner inp lose win|]) (?==win)]

ex8_tie = property $
  do names <- shuffle ["Mike","Bob","Lisa","Jane"]
     let (first:second:third:_) = names
     score <- choose (0,10000)
     thirdScore <- choose (0,20000)
     let inp = Map.fromList [(first,score),(second,score),(third,thirdScore)]
     return $ $(testing [|winner inp first second|]) (?==first)

ex9_bool = forAllBlind (choose (0,10)) $ \n ->
  forAllBlind (vectorOf n arbitrary) $ \bs ->
  $(testing [|freqs bs|]) . was $ \m ->
  let (t,f) = partition id bs
      ntrue = length t
      nfalse = length f
      out = Map.assocs m
  in conjoin [counterexample ("  Number of True values should be "++show ntrue) $
              null t || (True,ntrue) `elem` out
             ,counterexample ("  Number of False values should be "++show nfalse) $
              null f || (False,nfalse) `elem` out]

ex9_int = forAllBlind (choose (0,15)) $ \n ->
  forAllBlind (vectorOf n arbitrary) $ \(is::[Int]) ->
  $(testing [|freqs is|]) . was $ \out ->
  let vals = nub is
  in counterexample "  Number of keys" (length (Map.keys out) ?== length vals)
     .&&.
     conjoin (map (ck out is) vals)
  where ck out vals i = let exp = length (filter (==i) vals)
                        in counterexample ("  Should contain "++show(i,exp)) $ (i,exp) `elem` (Map.assocs out)

ex10_success = property $
  do names <- shuffle ["Mike","Bob","Lisa","Jane"]
     let (from:to:third:_) = names
     fromBal <- choose (0,100)
     toBal <- choose (0,100)
     thirdBal <- choose (0,100)
     amount <- choose (0,100)
     let inp = Map.fromList [(from,fromBal+amount),(to,toBal),(third,thirdBal)]
     let out = Map.fromList [(from,fromBal),(to,toBal+amount),(third,thirdBal)]
     return $ $(testing [|transfer from to amount inp|]) (?==out)

ex10_missing = property $
  do names <- shuffle ["Mike","Bob","Lisa","Jane"]
     let (present:missing:third:_) = names
     bal <- oneof [return 0, choose (0,100)]
     thirdBal <- choose (0,100)
     amount <- choose (0,100)
     let inp = Map.fromList [(present,bal),(third,thirdBal)]
     return $ conjoin [$(testing [|transfer present missing amount inp|]) (?==inp)
                      ,$(testing [|transfer missing present amount inp|]) (?==inp)]

ex10_notEnough = property $
  do names <- shuffle ["Mike","Bob","Lisa","Jane"]
     let (from:to:third:_) = names
     fromBal <- choose (0,100)
     toBal <- choose (0,100)
     thirdBal <- choose (0,100)
     amount <- choose (1,100)
     let inp = Map.fromList [(from,fromBal),(to,toBal),(third,thirdBal)]
     return $ conjoin [$(testing [|transfer from to (fromBal+amount) inp|]) (?==inp)
                      ,$(testing [|transfer from to (negate amount) inp|]) (?==inp)]

ex11_swap_small = property $
  do a <- choose ('a','z')
     b <- choose ('a','z')
     i <- choose (0,10::Int)
     let inp = listArray (i,i+1) [a,b]
         out = listArray (i,i+1) [b,a]
     return $ $(testing [|swap i (i+1) inp|]) (?==out)


ex11_swap_large = property $
  do base <- choose (0,10::Int)
     len <- choose (3,6::Int)
     let max = base+len-1
     vals <- shuffle (take len [0..])
     ~(i:j:_) <- shuffle [base..max]
     let vi = vals !! (i-base)
     let vj = vals !! (j-base)
     let inp = listArray (base,max) (vals::[Int])
     let out = listArray (base,max) (map (\x -> if x==vi then vj else if x == vj then vi else x) vals)
     return $ $(testing [|swap i j inp|]) (?==out)

ex12_bool = property $
  do small <- choose (0,100::Int)
     diff <- choose (1,100)
     return $ conjoin [$(testing [|maxIndex (array (False,True) [(False,small),(True,small+diff)])|]) (?==True)
                      ,$(testing [|maxIndex (array (False,True) [(True,small),(False,small+diff)])|]) (?==False)]

ex12_int = property $
  do low <- choose (0,100::Int)
     hi <- choose (low+1,low+10)
     mid <- choose (low,hi)
     val <- choose (0,100::Int)
     let inp = array (low,hi) [(i,if i==mid then val+1 else val) | i <- [low..hi]]
     return $ $(testing [|maxIndex inp|]) (?==mid)
