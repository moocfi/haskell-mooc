module Set16a where

import Mooc.Todo
import Test.QuickCheck

import Data.List

------------------------------------------------------------------------------
-- Ex 1: Write a Property that checks that a given list is sorted (in
-- ascending order)
--
-- Examples:
--  *Set16a> quickCheck (isSorted [1,2,3])
--  +++ OK, passed 1 test.
--  *Set16a> quickCheck (isSorted [1,3,2])
--  *** Failed! Falsified (after 1 test):
--  *Set16a> quickCheck (isSorted [])
--  +++ OK, passed 1 test.

isSorted :: (Show a, Ord a) => [a] -> Property
isSorted = todo

------------------------------------------------------------------------------
-- Ex 2: In this and the following exercises, we'll build a suite of
-- tests for testing a function
--
--   frequencies :: Eq a => [a] -> [(a,Int)]
--
-- That counts how many times each element occurs in a list.
--
-- The tests will be properties that check that a given input (of type
-- [a]) and output (of type [(a,Int)]) have a certain relationship.
--
-- For this exercise, implement a Property that checks that the sum
-- all the Ints in the output is the length of the input.
--
-- Examples:
-- (the exact output of the test doesn't matter, only whether it passes)
--
--  *Set16a> quickCheck (sumIsLength "abb" [('a',1),('b',2)])
--  +++ OK, passed 1 test.
--  *Set16a> quickCheck (sumIsLength "abb" [('a',1),('x',3),('z',-1)])
--  +++ OK, passed 1 test.
--  *Set16a> quickCheck (sumIsLength "abb" [('a',1),('b',1)])
--  *** Failed! Falsified (after 1 test):
--  3 /= 2
--  *Set16a> quickCheck (sumIsLength "" [])
--  +++ OK, passed 1 test.
--  *Set16a> quickCheck (sumIsLength [4,5,6,4,5,4] (freq1 [4,5,6,4,5,4]))
--  +++ OK, passed 1 test.

sumIsLength :: Show a => [a] -> [(a,Int)] -> Property
sumIsLength input output = todo

-- This is a function that passes the sumIsLength test but is wrong
freq1 :: Eq a => [a] -> [(a,Int)]
freq1 [] = []
freq1 [x] = [(x,1)]
freq1 (x:y:xs) = [(x,1),(y,length xs + 1)]

------------------------------------------------------------------------------
-- Ex 3: Implement a Property that takes an arbitrary element from the
-- input, and checks that it occurs in the output.
--
-- You can assume that the input is nonempty.
--
-- Hint: Use forAll to pick an element from the input.
--
-- Examples:
--  *Set16a> quickCheck (inputInOutput "abb" [('a',1),('b',2)])
--  +++ OK, passed 100 tests.
--  *Set16a> quickCheck (inputInOutput "abb" [('a',1),('x',3),('z',-1)])
--  *** Failed! Falsified (after 1 test):
--  'b'
--  *Set16a> quickCheck (inputInOutput [4,5,6,4,5,4] (freq1 [4,5,6,4,5,4]))
--  *** Failed! Falsified (after 3 tests):
--  6
--  *Set16a> quickCheck (inputInOutput [4,5,6,4,5,4] (freq2 [4,5,6,4,5,4]))
--  +++ OK, passed 100 tests.

inputInOutput :: (Show a, Eq a) => [a] -> [(a,Int)] -> Property
inputInOutput input output = todo

-- This function passes both the sumIsLength and inputInOutput tests
freq2 :: Eq a => [a] -> [(a,Int)]
freq2 xs = map (\x -> (x,1)) xs

------------------------------------------------------------------------------
-- Ex 4: Implement a Property that takes a pair (x,n) from the
-- output, and checks that x occurs n times in the input.
--
-- You can assume that the output is nonempty.
--
-- Examples:
--  *Set16a> quickCheck (outputInInput "abb" [('a',1)])
--  +++ OK, passed 100 tests.
--  *Set16a> quickCheck (outputInInput "abb" [('a',1),('x',3)])
--  *** Failed! Falsified (after 1 test):
--  ('x',3)
--  0 /= 3
--  *Set16a> quickCheck (outputInInput "abb" [('a',1),('b',3)])
--  *** Failed! Falsified (after 3 tests):
--  ('b',3)
--  2 /= 3
--  *Set16a> quickCheck (outputInInput [4,5,6,4,5,4] (freq2 [4,5,6,4,5,4]))
--  *** Failed! Falsified (after 1 test):
--  (4,1)
--  3 /= 1
--  *Set16a> quickCheck (outputInInput [4,5,6,4,5,4] (freq3 [4,5,6,4,5,4]))
--  +++ OK, passed 100 tests.

outputInInput :: (Show a, Eq a) => [a] -> [(a,Int)] -> Property
outputInInput input output = todo

-- This function passes the outputInInput test but not the others
freq3 :: Eq a => [a] -> [(a,Int)]
freq3 [] = []
freq3 (x:xs) = [(x,1 + length (filter (==x) xs))]

------------------------------------------------------------------------------
-- Ex 5: Implement a Property that takes a candidate function freq, a
-- NonEmptyList Char input, and checks that all the three properties
-- (sumIsLength, inputInOutput and outputInInput) hold between the
-- input and the output from freq.
--
-- Hint: Use conjoin or (.&&.) to "and" together properties.
--
-- Examples:
--  *Set16a> quickCheck (frequenciesProp freq1)
--  *** Failed! Falsified (after 5 tests and 5 shrinks):
--  ...
--  *Set16a> quickCheck (frequenciesProp freq2)
--  *** Failed! Falsified (after 20 tests and 2 shrinks):
--  ...
--  *Set16a> quickCheck (frequenciesProp freq3)
--  *** Failed! Falsified (after 1 test and 2 shrinks):
--  ...
--  *Set16a> quickCheck (frequenciesProp frequencies)
--  +++ OK, passed 100 tests.

frequenciesProp :: ([Char] -> [(Char,Int)]) -> NonEmptyList Char -> Property
frequenciesProp freq input = todo

frequencies :: Eq a => [a] -> [(a,Int)]
frequencies [] = []
frequencies (x:ys) = (x, length xs) : frequencies others
  where (xs,others) = partition (==x) (x:ys)

------------------------------------------------------------------------------
-- Ex 6: Write a generator for lists that have these properties:
-- * length 3-5
-- * elements are numbers from 0 to 10
-- * the list is sorted
--
-- Hints: Remember the generators `elements` and `choose`, but also
-- check out `vectorOf`, and don't be afraid to use `sort`.
--
-- Example:
--  *Set16a> sample genList
--  [0,1,2,8]
--  [3,5,7,8,10]
--  [6,7,9]
--  [2,2,3]
--  [0,5,7,10]
--  [5,8,10]
--  [1,6,8]
--  [1,5,8,9]
--  [2,3,4,6,8]
--  [0,1,8]
--  [2,4,10]

genList :: Gen [Int]
genList = todo

------------------------------------------------------------------------------
-- Ex 7: Here are the datatypes Arg and Expression from Set 15. Write
-- Arbitrary instances for Expression and Arg such that:
--
-- * All combinations of Plus, Minus, Number, Variable are produced
-- * All numbers are in the range 0-10
-- * All variables are one of a,b,c,x,y,z
--
-- You don't need to implement shrink, just arbitrary.
--
-- Hint: use
--   oneof :: [Gen a] -> Gen a
-- to randomly pick from a set of generators
--
-- Examples:
--  *Set16a> sample (arbitrary :: Gen Arg)
--  Number 3
--  Variable 'c'
--  Variable 'z'
--  Number 5
--  Number 6
--  Variable 'x'
--  *Set16a> sample (arbitrary :: Gen Expression)
--  Minus (Variable 'b') (Variable 'c')
--  Plus (Variable 'c') (Number 0)
--  Minus (Number 0) (Variable 'c')
--  Minus (Number 0) (Number 7)
--  Minus (Number 8) (Number 5)

data Arg = Number Int | Variable Char
  deriving (Show, Eq)

data Expression = Plus Arg Arg | Minus Arg Arg
  deriving (Show, Eq)

instance Arbitrary Arg where
  arbitrary = todo

instance Arbitrary Expression where
  arbitrary = todo
