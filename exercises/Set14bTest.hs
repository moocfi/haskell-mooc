{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TupleSections #-}

module Set14bTest where

import Mooc.Test
import Mooc.Th

import Control.Exception
import Control.Monad
import Data.List
import qualified Data.Text as T
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Database.SQLite.Simple

import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Network.Wai.Handler.Warp (withApplication, testWithApplication)

import Set14b hiding (main)

main = score tests

tests = [(1,"deposit",[ex1])
        ,(2,"withdraw",[ex2_small, ex2_big])
        ,(3,"parseCommand",[ex3])
        ,(4,"perform",[ex4_small, ex4_big])
        ,(5,"simpleServer",[ex5])
        ,(6,"server",[ex6_small, ex6_big])
        ,(7,"server withdraw",[ex7_small, ex7_big])
        ,(8,"server isError",[ex8_parseCommand, ex8_http])]

-- -- -- -- --

word = listOf1 (choose ('a','z'))

getAllQuery' = Query (T.pack "SELECT account, amount FROM events;")

-- TODO test against SQL injections?
ex1 =
  forAllBlind word $ \name1 ->
  forAllBlind word $ \name2 ->
  forAllBlind (choose (0,10)) $ \amount1 ->
  forAllBlind (choose (0,10)) $ \amount2 ->
  counterexample "After running" $
  counterexample ("  db <- openDatabase \"\"") $
  counterexample ("  deposit db (T.pack " ++ show name1 ++ ") " ++ show amount1) $
  counterexample ("  deposit db (T.pack " ++ show name2 ++ ") " ++ show amount2) $
  monadicIO $ do
  out <- run $ bracket (openDatabase "") close $ \db -> do
    deposit db (T.pack name1) amount1
    deposit db (T.pack name2) amount2
    query_ db getAllQuery' :: IO [(String,Int)]
  stop_ $ counterexample "The output of: query_ db getAllQuery :: IO [(String,Int)]" $
    out ?== [(name1,amount1),(name2,amount2)]

ex2_small =
  forAllBlind word $ \name ->
  forAllBlind (choose (0,10)) $ \amount1 ->
  forAllBlind (choose (0,10)) $ \amount2 ->
  counterexample "After running" $
  counterexample ("  db <- openDatabase \"\"") $
  counterexample ("  deposit db (T.pack " ++ show name ++ ") " ++ show amount1) $
  counterexample ("  deposit db (T.pack " ++ show name ++ ") " ++ show amount2) $
  counterexample ("The output of: balance db " ++ show name) $
  monadicIO $ do
  out <- run $ bracket (openDatabase "") close $ \db -> do
    deposit db (T.pack name) amount1
    deposit db (T.pack name) amount2
    balance db (T.pack name)
  stop_ $ out ?== amount1 + amount2

ex2_big =
  forAllBlind word $ \name ->
  forAllBlind (listOf (choose (0,10))) $ \amounts ->
  forAllBlind (listOf1 $ (,) <$> suchThat word (/=name) <*> choose (0,10)) $ \chaff ->
  let input0 = map (name,) amounts ++ chaff
  in forAllBlind (shuffle input0) $ \input ->
    counterexample "After running" $
    counterexample (intercalate "\n" (map depo input)) $
    counterexample ("The output of: balance db " ++ show name) $
    monadicIO $ do
    out <- run $ bracket (openDatabase "") close $ \db -> do
      forM_ input $ \(n,a) -> deposit db (T.pack n) a
      balance db (T.pack name)
    stop_ $ out ?== sum amounts
  where depo (name,amount) = "  deposit db " ++ show name ++ " " ++ show amount

ex3 =
  forAllBlind word $ \name ->
  forAllBlind (choose (0,9999)) $ \amount ->
  conjoin [counterexample ("parseCommand [T.pack \"balance\", T.pack " ++ show name ++ "]") $
           parseCommand [T.pack "balance", T.pack name]
            ?== Just (Balance (T.pack name))
          ,counterexample ("parseCommand [T.pack \"deposit\", T.pack " ++ show name ++ ", T.pack " ++ show (show amount) ++ "]") $
           parseCommand [T.pack "deposit", T.pack name, T.pack (show amount)]
            ?== Just (Deposit (T.pack name) amount)]

ex4_small =
  forAllBlind word $ \name ->
  forAllBlind (choose (0,10)) $ \amount ->
  counterexample "Running" $
  counterexample "  db <- openDatabase \"\"" $
  counterexample ("  out1 <- perform db (Just (Deposit (T.pack "++ show name++") " ++ show amount ++ "))") $
  counterexample ("  out2 <- perform db (Just (Balance (T.pack "++ show name++")))") $
  counterexample ("  return [out1,out2]") $
  counterexample "The output:" $
  monadicIO $ do
  out <- run $ bracket (openDatabase "") close $ \db -> do
    out1 <- perform db (Just (Deposit (T.pack name) amount))
    out2 <- perform db (Just (Balance (T.pack name)))
    return [out1,out2]
  stop_ $ out ?== [T.pack "OK", T.pack (show amount)]

mkDeposit n a = Just (Deposit (T.pack n) a)

ex4_big =
  forAllBlind word $ \name ->
  forAllBlind (listOf (choose (0,10))) $ \amounts ->
  forAllBlind (listOf1 $ mkDeposit <$> suchThat word (/=name) <*> choose (0,10)) $ \chaff ->
  let input0 = map (mkDeposit name) amounts ++ chaff
  in forAllBlind (shuffle input0) $ \input1 ->
    let input = input1 ++ [Just (Balance (T.pack name))]
    in counterexample "Note: T.pack calls left out due to technical reasons." $
       counterexample "Running" $
       counterexample "  db <- openDatabase \"\"" $
       counterexample ("  mapM (perform db) " ++ show input) $
       monadicIO $ do
        out <- run $ bracket (openDatabase "") close $ \db ->
          mapM (perform db) input
        stop_ $ out ?== (replicate (length input - 1) (T.pack "OK")) ++ [T.pack (show (sum amounts))]

get url = simpleHTTP (getRequest url) >>= getResponseBody

ex5 =
  once $
  counterexample "Running: run 8899 simpleServer" $
  counterexample "And fetching http://localhost:8899" $
  monadicIO $ do
  out <- run $ testWithApplication (return simpleServer) $ \port ->
    get ("http://127.0.0.1:" ++ show port)
  stop_ $ out ?== "BANK"

testHTTP paths expected =
  counterexample "Running:" $
  counterexample "  db <- openDatabase \"\"" $
  counterexample "  run 4321 (server db)" $
  counterexample "Fetching:" $
  counterexample (unlines $ map ("  - http://localhost:4321"++) paths) $
  monadicIO $ do
  out <- run $ bracket (openDatabase "") close $ \db ->
    testWithApplication (return (server db)) $ \port ->
    mapM get (map (("http://127.0.0.1:"++show port)++) paths)
  stop_ $ out ?== expected

ex6_small =
  once $
  testHTTP ["/deposit/lopez/17", "/balance/lopez"] ["OK", "17"]

depositPath :: String -> Int -> String
depositPath name amount
  | amount<0 = "/withdraw/" ++ name ++ "/" ++ show (negate amount)
  | otherwise = "/deposit/" ++ name ++ "/" ++ show amount

ex6_big =
  forAllBlind word $ \name ->
  forAllBlind (listOf (choose (0,10))) $ \amounts ->
  forAllBlind (listOf1 $ depositPath <$> suchThat word (/=name) <*> choose (0,10)) $ \chaff ->
  let input0 = map (depositPath name) amounts ++ chaff
  in forAllBlind (shuffle input0) $ \input1 ->
    let input = input1 ++ ["/balance/" ++ name]
    in testHTTP input ((replicate (length input - 1) "OK") ++ [show (sum amounts)])

ex7_small =
  once $
  testHTTP ["/deposit/simon/17", "/withdraw/simon/6", "/balance/simon"] ["OK", "OK", "11"]

ex7_big =
  forAllBlind word $ \name ->
  forAllBlind (listOf (choose (-10,10))) $ \amounts ->
  forAllBlind (listOf1 $ depositPath <$> suchThat word (/=name) <*> choose (0,10)) $ \chaff ->
  let input0 = map (depositPath name) amounts ++ chaff
  in forAllBlind (shuffle input0) $ \input1 ->
    let input = input1 ++ ["/balance/" ++ name]
    in testHTTP input ((replicate (length input - 1) "OK") ++ [show (sum amounts)])

ex8_parseCommand = conjoin [$(testing [|parseCommand v|]) (?==Nothing)
                           | v <- (map.map) T.pack [[]
                                                   ,["deposit"]
                                                   ,["withdraw"]
                                                   ,["deposit","x"]
                                                   ,["withdraw","x"]
                                                   ,["deposit","x","123x"]
                                                   ,["withdraw","x","123x"]
                                                   ,["deposit","x","123","x"]
                                                   ,["withdraw","x","123","x"]
                                                   ,["balance"]
                                                   ,["balance","x","y"]
                                                   ,["balance","x","123"]]]

isError url = testHTTP [url] ["ERROR"]

ex8_http = forAllBlind word $ \w ->
  forAllBlind word $ \name ->
  forAllBlind (choose (0,10::Int)) $ \amount ->
  conjoin [isError ("/"++w)
          ,isError ("/deposit/"++w)
          ,isError ("/withdraw/"++w)
          ,isError ("/deposit/"++name++"/"++show amount++w)
          ,isError ("/withdraw/"++name++"/"++show amount++w)
          ,isError ("/deposit/"++name++"/"++show amount++"/"++w)
          ,isError ("/deposit/"++name++"/"++show amount++"/"++w++"/"++w)
          ,isError ("/withdraw/"++name++"/"++show amount++"/"++w)
          ,isError ("/balance")
          ,isError ("/balance/" ++ name ++ "/" ++ w)
          ,isError ("/balance/" ++ name ++ "/" ++ show amount)]
