{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Set11aTest where

import Mooc.Test
import Mooc.Th

import Control.Monad
import Data.List
import Data.IORef
import GHC.IO.Handle
import System.Directory
import System.IO

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Set11b hiding (counter)

main = score tests

tests = [(1,"appendAll",[ex1])
        ,(2,"swapIORefs",[ex2])
        ,(3,"doubleCall",[ex3_doubleCall, ex3_doubleCall_2])
        ,(4,"composeIO",[ex4_composeIO_1, ex4_composeIO_2])
        ,(5,"hFetchLines",[ex5])
        ,(6,"hSelectLines",[ex6_hSelectLines])
        ,(7,"interact'",[ex7_interact_terminates, ex7_interact_example, ex7_interact_loop])]

-- -- -- -- --

word = listOf1 (choose ('a','z'))

ex1 = forAllBlind word $ \init ->
  forAllBlind (listOf word) $ \ws ->
  counterexample ("appendAll ref " ++ show ws) $
  counterexample (" Initial value of ref was: " ++ show init) $
  monadicIO $ do ref <- run $ newIORef init
                 run $ appendAll ref ws
                 out <- run $ readIORef ref
                 stop_ $ counterexample " Final value in ref" $ out ?== concat (init:ws)

ex2 = forAllBlind word $ \w1 ->
  forAllBlind word $ \w2 ->
  counterexample "swapIORefs r1 r2" $
  counterexample (" Initial value of r1: "++show w1) $
  counterexample (" Initial value of r2: "++show w2) $
  monadicIO $ do r1 <- run $ newIORef w1
                 r2 <- run $ newIORef w2
                 run $ swapIORefs r1 r2
                 o1 <- run $ readIORef r1
                 o2 <- run $ readIORef r2
                 stop_ $ conjoin [counterexample " readIORef r1" $ o1 ?== w2
                                 ,counterexample " readIORef r2" $ o2 ?== w1]

ex3_doubleCall =
  forAllBlind (choose (0,20::Int)) $ \i ->
  counterexample ("doubleCall (return (return "++show i++"))") $
  ($ doubleCall (return (return i))) . withNoInput $
  check (?=="") (?==i)

ex3_doubleCall_2 =
  forAllBlind word $ \w1 ->
  forAllBlind word $ \w2 ->
  counterexample ("doubleCall (do putStrLn "++show w1++"; return (putStrLn "++show w2++"))") $
  ($ doubleCall (do putStrLn w1; return (putStrLn w2))) . withNoInput $
  check (?==(w1++"\n"++w2++"\n")) (?==())

ex4_composeIO_1 =
  forAllBlind word $ \w ->
  forAllBlind word $ \v ->
  counterexample ("compose putStrLn (\\x -> do putStrLn x; getLine) " ++ show w) $
  ($ compose putStrLn (\x -> do putStrLn x; getLine) w) . withInput (v++"\n") $
  checkOutput (?==w++"\n"++v++"\n")

ex4_composeIO_2 =
  forAll_ $ \(i::Int) ->
  counterexample ("compose (\\x -> return (x*2)) (\\y -> return (y+1)) " ++ show i) $
  ($ compose (return . (*2)) (return . (+1)) i) . withNoInput $
    checkResult (?== (i+1)*2)

ex5 = forAllBlind (listOf1 word) $ \lines ->
  counterexample ("Contents of handle h:\n"++unlines lines) $
  counterexample "hFetchLines h" $
  monadicIO $ do
  dir <- run $ getTemporaryDirectory
  (_,h) <- run $ openTempFile dir "hSelectLines.in"
  run $ hPutStr h $ unlines lines
  run $ hSeek h AbsoluteSeek 0
  outs <- run $ hFetchLines h
  length outs `seq` do
    run $ hClose h
    stop_ $ outs ?== lines

ex6_hSelectLines =
  forAllBlind (listOf1 word) $ \lines ->
  forAllBlind (fmap (nub.sort) . listOf1 $ choose (1,length lines)) $ \inds ->
  counterexample ("Contents of handle h:\n"++unlines lines) $
  counterexample ("hSelectLines h "++show inds) $
  monadicIO $ do
  dir <- run $ getTemporaryDirectory
  (path,h) <- run $ openTempFile dir "hSelectLines.in"
  run $ hPutStr h $ unlines lines
  run $ hSeek h AbsoluteSeek 0
  outs <- run $ hSelectLines h inds
  length (concat outs) `seq` do
    run $ hClose h
    stop_ $ counterexample ("  was: "++show outs) $
      conjoin [counterexample "Length of result" $ length outs ?== length inds
              ,conjoin [counterexample ("result!!"++show i) $ outs!!i ?== lines!!((inds!!i)-1)
                       | i <- [0..length inds-1]]]

counter :: (String,Integer) -> (Bool,String,Integer)
counter ("inc",n)   = (True,"done",n+1)
counter ("print",n) = (True,show n,n)
counter ("quit",n)  = (False,"bye bye",n)

ex7_interact_terminates =
  forAllBlind word $ \w1 ->
  forAllBlind word $ \w2 ->
  forAllBlind (choose (0::Int,100)) $ \state ->
  counterexample ("let f (input,state) = (False,input,state) in interact' f "++show state) $
  ($ let f (input,state) = (False,input,state) in interact' f state) . withInput (unlines [w1,w2]) $
  check (?==(w1++"\n")) (?==state)

ex7_interact_example =
  $(testing' [|interact' counter 1|]) . withInput (unlines ["print","inc","inc","print","quit"]) $
  check (?==unlines ["1","done","done","3","bye bye"]) (?==3)

ex7_interact_loop =
  forAllBlind (choose (0::Int,100)) $ \state ->
  forAllBlind (choose (0::Int,5)) $ \steps ->
  forAllBlind word $ \w ->
  forAllBlind (word `suchThat` (/=w)) $ \end ->
  let input = unlines $ replicate steps w ++ [end]
      output = unlines $ map show [state..state+steps]
  in counterexample ("let f (input,state) = (input=="++show w++",show state,state+1) in interact' f "++show state) $
     ($ let f (input,state) = (input==w,show state,state+1) in interact' f state) . withInput input $
     check (?==output) (?==state+steps+1)
