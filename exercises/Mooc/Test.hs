{-# LANGUAGE CPP, ScopedTypeVariables, TemplateHaskell #-}

module Mooc.Test where

import Control.Exception (evaluate,SomeException,fromException)
import Control.Monad
import Control.DeepSeq (deepseq)
import Data.Foldable
import Data.Functor
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Semigroup
import System.Environment
import System.Timeout
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Mooc.Todo

-- better assertions

-- strict counterexample', in case showing some values causes errors or never terminates
counterexample' string prop = string `deepseq` counterexample string prop

expectation expected actual = counterexample' ("  Expected: " ++ show expected ++ "\n  Was: " ++ show actual)

expected ==? actual = expectation expected actual (expected == actual)
actual ?== expected = expected ==? actual

infix 4 ==?
infix 4 ?==

actual ?~= expected = expectation expected actual (abs (actual-expected) < 0.01)
expected =~? actual = actual ?~= expected

infix 4 =~?
infix 4 ?~=

hasElements expected actual = counterexample' ("  Expected elements (in any order): " ++ show expected
                                               ++ "\n  Was: " ++ show actual)
                              (sort expected == sort actual)

was f actual = counterexample' ("  Was: "++show actual) (f actual)

-- helpers

shrinkPositive :: Int -> [Int]
shrinkPositive = map getPositive . shrink . Positive

forAllShrink_ :: Arbitrary a => Gen a -> (a -> Property) -> Property
forAllShrink_ gen = forAllShrinkBlind gen shrink

forAll_ :: Arbitrary a => (a -> Property) -> Property
forAll_ = forAllShrink_ arbitrary

-- timeouts for evaluation

timedMillis = 500

timed val k = monadicIO $ do
  res <- run $ timeout (timedMillis*1000) $ evaluate val
  case res of
    Nothing -> return $ counterexample' ("  didn't return in "++show timedMillis++"ms") $ False
    Just v -> return $ k v

-- handling TODO excercises

isTodo :: SomeException -> Bool
isTodo e = case fromException e of Just TODO -> True
                                   _ -> False

resultIsTodo :: Result -> Bool
resultIsTodo (Failure {theException = (Just e)}) = isTodo e
resultIsTodo _ = False

-- scoring

data Outcome = Pass | Fail | Todo deriving (Show, Eq, Ord)

instance Semigroup Outcome where
  (<>) = max

instance Monoid Outcome where
  mempty = Pass

quietArgs = stdArgs {chatty=False}

timeLimit = 10 * 1000 * 1000 -- 10 seconds in microseconds

myCheck :: Testable prop => prop -> IO Outcome
myCheck prop = quickCheckWithResult quietArgs (within timeLimit prop) >>= interpret
  where interpret res
          | resultIsTodo res = return Todo
          | isSuccess res = return Pass
          | otherwise = putStrLn (output res) >> return Fail

histogram vals = [(head xs, length xs) | xs <- group (sort vals)]

textAttr False a s = s
textAttr True a s = "\ESC[" ++ a ++ "m" ++ s ++ "\ESC[0m"
textBold color = textAttr color "1"
textBoldGreen color = textAttr color "1;32"
textBoldRed color = textAttr color "1;31"
textBlue color = textAttr color "34"

showCheck :: Bool -> Outcome -> String
showCheck color Todo = "_"
showCheck color Pass = textBoldGreen color "1"
showCheck color Fail = textBoldRed color "0"

showOutcome :: Bool -> Outcome -> String
showOutcome color Pass = textBoldGreen color "+++++ Pass"
showOutcome color Fail = textBoldRed color "----- Fail"
showOutcome color Todo = textBlue color "00000 Todo"

showExercise :: Bool -> Int -> String
showExercise color i = textBold color $ "===== EXERCISE " ++ show i

showFinal :: Bool -> [Outcome] -> String
showFinal color outs = concatMap (showCheck color) outs ++ "\n" ++ show score ++ " / " ++ show total
  where score = length $ filter (==Pass) outs
        total = length outs

type Test = (Int,String,[Property])

toJSON :: [(Int,String,Outcome)] -> String
toJSON ts = "[" ++ intercalate "," (map f ts) ++ "]"
  where f (i,name,outcome) = "{\"number\":"++show i++",\"name\":"++show name++",\"outcome\":\""++show outcome++"\"}"

score :: [Test] -> IO ()
score tests = do
  color <- fmap isNothing $ lookupEnv "NO_COLOR"
  triples <- forM tests $ \(i,n,ts) -> do
    putStrLn $ showExercise color i
    out <- fold <$> mapM myCheck ts
    putStrLn $ showOutcome color out
    return (i,n,out)
  let outcomes = [o | (_,_,o) <- triples]
  putStrLn "===== TOTAL"
  putStrLn $ showFinal color outcomes
  writeFile "score.json" $ toJSON triples
