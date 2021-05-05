{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Set14aTest where

import Mooc.Test
import Mooc.Th

import Data.Bits
import Data.Char
import Data.List
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding
import Test.QuickCheck


import Set14a

main = score tests

tests = [(1,"greetText",[ex1_short,ex1_long])
        ,(2,"shout",[ex2])
        ,(3,"longestRepeat",[ex3])
        ,(4,"takeStrict",[ex4])
        ,(5,"byteRange",[ex5])
        ,(6,"xorChecksum",[ex6_simple, ex6])
        ,(7,"countUtf8Chars",[ex7_ok, ex7_fail])
        ,(8,"pingpong",[ex8])]

-- -- -- -- --

letter = choose ('a','z')

ex1_short = forAllBlind (choose (0,15)) $ \n ->
  forAllBlind (vectorOf n letter) $ \name ->
  counterexample ("greetText (T.pack " ++ show name ++ ")") $
  greetText (T.pack name) ?== T.pack ("Hello, "++name++"!")

ex1_long = forAllBlind (vectorOf 15 letter) $ \name ->
  forAllBlind (listOf1 letter) $ \suffix ->
  counterexample ("greetText (T.pack " ++ show (name++suffix) ++ ")") $
  greetText (T.pack (name++suffix)) ?== T.pack ("Hello, "++name++"...!")

shuf (x:xs) ys = x:shuf ys xs
shuf [] _ = []

word = listOf1 letter

ex2 = forAllBlind (listOf word) $ \up0 ->
  forAllBlind (listOf word) $ \low ->
  let up = map (map toUpper) up0
      inp = unwords $ shuf up0 low
      out = unwords $ shuf up low
  in counterexample ("shout (T.pack " ++ show inp ++ ")") $
  shout (T.pack inp) ?== T.pack out

noRepeats s = group s == map (:[]) s

ex3 = forAllBlind (listOf (choose (1,8))) $ \is ->
  forAllBlind (vectorOf (length is) letter `suchThat` noRepeats) $ \cs ->
  let inp = concat $ zipWith replicate is cs
      out = maximum (0:is)
  in counterexample' ("longestRepeat (T.pack "++show inp++")") $
  within timeLimit $
  longestRepeat (T.pack inp) ?== out

ex4 = forAllBlind word $ \w ->
  forAllShrink_ (choose (0,200)) $ \n ->
  (n>=0 ==>) $
  counterexample' ("takeStrict "++show n++" (TL.pack (cycle "++show w++"))") $
  within timeLimit $
  takeStrict n (TL.pack (cycle w)) ?== T.pack (take (fromIntegral n) (cycle w))

ex5 = forAll_ $ \bytes ->
  counterexample' ("byteRange (B.pack "++show bytes++")") $
  byteRange (B.pack bytes) ?== if null bytes then 0 else maximum bytes - minimum bytes

ex6_simple = once $
  conjoin [counterexample "xorChecksum (B.pack [])" $
           xorChecksum (B.pack []) ?== 0
          ,counterexample "xorChecksum (B.pack [17])" $
           xorChecksum (B.pack [17]) ?== 17]

ex6 = forAll_ $ \w1 ->
  forAll_ $ \w2 ->
  forAll_ $ \w3 ->
  forAll_ $ \chaff ->
  forAllBlind (shuffle ([w1,w2,w3]++chaff++chaff)) $ \input ->
  counterexample' ("xorChecksum (B.pack "++show input++")") $
  xorChecksum (B.pack input) ?== xor w1 (xor w2 w3)

exoticLetter = elements "åäö€°∑"
maybeExoticLetter = elements "xyåäö€°∑"


encode = B.unpack . encodeUtf8 . T.pack

ex7_ok = forAllBlind (choose (0,10)) $ \l ->
  forAllBlind (vectorOf l maybeExoticLetter) $ \s ->
  counterexample' ("countUtf8Chars (encodeUtf8 (T.pack "++show s++"))") $
  countUtf8Chars (encodeUtf8 (T.pack s)) ?== Just l

fault s = do
  c <- elements s
  return $ delete c s

ex7_fail =
  forAllBlind (choose (1,5)) $ \l ->
  forAllBlind (vectorOf l exoticLetter) $ \s ->
  let ok = encode s
  in forAllBlind (fault ok) $ \inp ->
    counterexample' ("countUtf8Chars (B.pack "++show inp++")") $
    countUtf8Chars (B.pack inp) ?== Nothing

ex8 =
  forAll_ $ \(NonEmpty bs) ->
  counterexample' ("pingpong (B.pack "++show bs++")") $
  let n = length bs
      out = pingpong (B.pack bs)
  in conjoin [counterexample' ("  First bytes") $
              take (n * 3) (BL.unpack out) ?== (bs ++ reverse bs ++ bs)
             ,forAllBlind (choose (0,10000)) $ \i ->
              counterexample' ("  Byte at index "++show i) $
              BL.index out i ?== (bs ++ reverse bs) `genericIndex` (mod i (fromIntegral n * 2))]
