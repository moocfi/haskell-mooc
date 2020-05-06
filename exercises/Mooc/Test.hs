{-# LANGUAGE CPP, ScopedTypeVariables, TemplateHaskell #-}

module Mooc.Test where

import Control.Exception (evaluate,SomeException,fromException)
import Control.Monad
import Data.Foldable
import Data.Functor
import Data.List
import Data.Monoid
import Data.Semigroup
import System.Timeout
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Mooc.Todo

-- better assertions

expectation expected actual = counterexample ("  Expected: " ++ show expected ++ "\n  Was: " ++ show actual)

expected ==? actual = expectation expected actual (expected == actual)
actual ?== expected = expected ==? actual

infix 4 ==?
infix 4 ?==

actual ?~= expected = expectation expected actual (abs (actual-expected) < 0.01)
expected =~? actual = actual ?~= expected

infix 4 =~?
infix 4 ?~=

hasElements expected actual = counterexample ("  Expected elements (in any order): " ++ show expected
                                               ++ "\n  Was: " ++ show actual)
                              (sort expected == sort actual)

was f actual = counterexample ("  Was: "++show actual) (f actual)

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
    Nothing -> return $ counterexample ("  didn't return in "++show timedMillis++"ms") $ False
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

myCheck :: Testable prop => prop -> IO Outcome
myCheck prop = quickCheckWithResult quietArgs prop >>= interpret
  where interpret res
          | resultIsTodo res = return Todo
          | isSuccess res = return Pass
          | otherwise = putStrLn (output res) >> return Fail

histogram vals = [(head xs, length xs) | xs <- group (sort vals)]

textAttr a s = "\ESC[" ++ a ++ "m" ++ s ++ "\ESC[0m"
textBold = textAttr "1"
textBoldGreen = textAttr "1;32"
textBoldRed = textAttr "1;31"
textBlue = textAttr "34"

showCheck :: Outcome -> String
showCheck Todo = "_"
showCheck Pass = textBoldGreen "\x2713"
showCheck Fail = textBoldRed "\x2715"

showOutcome :: Outcome -> String
showOutcome Pass = textBoldGreen "+++++ Pass"
showOutcome Fail = textBoldRed "----- Fail"
showOutcome Todo = textBlue "00000 Todo"

showExercise :: Int -> String
showExercise i = textBold $ "===== EXERCISE " ++ show i

showFinal :: [Outcome] -> String
showFinal outs = concatMap showCheck outs ++ "\n" ++ show score ++ " / " ++ show total
  where score = length $ filter (==Pass) outs
        total = length outs

type Test = (Int,String,[Property])

toJSON :: [(Int,String,Outcome)] -> String
toJSON ts = "[" ++ intercalate "," (map f ts) ++ "]"
  where f (i,name,outcome) = "{\"number\":"++show i++",\"name\":"++show name++",\"outcome\":\""++show outcome++"\"}"

score :: [Test] -> IO ()
score tests = do
  triples <- forM tests $ \(i,n,ts) -> do
    putStrLn $ showExercise i
    out <- fold <$> mapM myCheck ts
    putStrLn $ showOutcome out
    return (i,n,out)
  let outcomes = [o | (_,_,o) <- triples]
  putStrLn "===== TOTAL"
  putStrLn $ showFinal outcomes
  writeFile "score.json" $ toJSON triples
