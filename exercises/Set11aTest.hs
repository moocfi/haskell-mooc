{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Set11aTest where

import Mooc.Test
import Mooc.Th

import Control.Monad
import Data.List

import Test.QuickCheck

import Set11a hiding (ask)

main = score tests

tests = precondition imports
  [(1,"hello",[ex1_hello])
  ,(2,"greetIO",[ex2_greet])
  ,(3,"greetIO2",[ex3_greet2])
  ,(4,"readWords",[ex4_readWords])
  ,(5,"readUntil",[ex5_readUntil])
  ,(6,"countdownPrint",[ex6])
  ,(7,"isums",[ex7_isums])
  ,(8,"whenM",[ex8_whenM_True, ex8_whenM_False])
  ,(9,"whileIO",[ex9_example, ex9_False])
  ,(10,"debug",[ex10_debug])]

-- -- -- -- --

imports = $(importsOnly "Set11a" ["Control.Monad","Data.List","Data.Foldable","Prelude","Data.OldList","System.IO"
                                 ,"Text.Read"
                                 ,"GHC.Num", "GHC.Base", "GHC.Classes", "GHC.Show", "GHC.Types", "GHC.Err", "GHC.List"
                                 ,"Mooc.Todo"])

ex1_hello =
  $(testing [|hello|]) . withNoInput $
  checkOutput (?== "HELLO\nWORLD\n")

word = listOf1 (choose ('a','z'))

ex2_greet = forAllBlind word $ \name ->
  $(testing [|greet name|]) . withNoInput $
  checkOutput (?== ("HELLO "++name++"\n"))

ex3_greet2 =  forAllBlind word $ \name ->
  $(testing [|greet2|]) . withInput (name++"\n") $
  checkOutput (?== ("HELLO "++name++"\n"))

ex4_readWords =
  forAllBlind (listOf1 word) $ \words ->
  $(testing [|readWords (length words - 1)|]) . withInput (unlines words) $
  checkResult (?== sort (init words))

ex5_readUntil =
  forAllBlind word $ \end ->
  forAllBlind (listOf1 (word `suchThat` (/=end))) $ \words ->
  counterexample ("readUntil (=="++show end++")") $
  ($ readUntil (==end)) $
  withInput (unlines $ words ++ [end]) $
  checkResult (?==words)

ex6 =
  forAllBlind (choose (0,40)) $ \n ->
  $(testing [|countdownPrint n|]) . withNoInput $
  checkOutput (?== unlines (map show [n,n-1..0]))

ex7_isums =
  forAllBlind (listOf1 (choose (-10,10))) $ \nums ->
  $(testing [|isums (length nums)|]) . withInput (unlines $ map show nums) $
  check (?==unlines (map show $ scanl1 (+) nums)) (?==sum nums)

ex8_whenM_True =
  forAllBlind (choose (0::Int,10)) $ \i ->
  counterexample ("with i = " ++ show i) $
  $(testing' [|whenM (return True) (print i)|]) . withNoInput $
  checkOutput (?== show i++"\n")

ex8_whenM_False =
  forAllBlind (choose (0::Int,10)) $ \i ->
  counterexample ("with i = " ++ show i) $
  $(testing' [|whenM (return False) (print i)|]) . withNoInput $
  checkOutput (?== "")

ask :: IO Bool
ask = do putStrLn "Y/N?"
         line <- getLine
         return $ line == "Y"

ex9_example =
  forAllBlind (choose (0,10)) $ \i ->
  $(testing' [|while ask (putStrLn "YAY!")|]) . withInput (unlines $ replicate i "Y" ++ ["N"]) $
  checkOutput (?== unlines (concat (replicate i ["Y/N?","YAY!"]) ++ ["Y/N?"]))

ex9_False =
  $(testing' [|while (return False) (putStrLn "IMPOSSIBLE")|]) . withNoInput $
  checkOutput (?== "")

ex10_debug =
  forAllBlind word $ \message ->
  forAllBlind word $ \output ->
  forAllBlind word $ \result ->
  counterexample ("debug "++show message++" (do putStrLn "++show output++"; return "++show result++")") $
  ($ (debug message (putStrLn output >> return result))) . withNoInput $
  check (?== unlines [message,output,message]) (?== result)
