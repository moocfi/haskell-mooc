-- Exercise set 6: defining classes and instances


module Set6 where

import Mooc.Todo
import Data.Char (toLower)

------------------------------------------------------------------------------
-- Ex 1: define an Eq instance for the type Country below. You'll need
-- to use pattern matching.

data Country = Finland | Switzerland | Norway
  deriving Show

instance Eq Country where
  (==) = todo

------------------------------------------------------------------------------
-- Ex 2: implement an Ord instance for Country so that
--   Finland < Norway < Switzerland
--
-- Remember minimal complete definitions!

instance Ord Country where
  compare = todo -- implement me?
  (<=) = todo -- and me?
  min = todo -- and me?
  max = todo -- and me?

------------------------------------------------------------------------------
-- Ex 3: Implement an Eq instance for the type Name which contains a String.
-- The Eq instance should ignore capitalization.
--
-- Hint: use the function Data.Char.toLower that has been imported for you.
--
-- Examples:
--   Name "Pekka" == Name "pekka"   ==> True
--   Name "Pekka!" == Name "pekka"  ==> False

data Name = Name String
  deriving Show

instance Eq Name where
  (==) = todo

------------------------------------------------------------------------------
-- Ex 4: here is a list type parameterized over the type it contains.
-- Implement an instance "Eq (List a)" that compares the lists element
-- by element.
--
-- Note how the instance needs an Eq a constraint. What happens if you
-- remove it?

data List a = Empty | LNode a (List a)
  deriving Show

instance Eq a => Eq (List a) where
  (==) = todo

------------------------------------------------------------------------------
-- Ex 5: below you'll find two datatypes, Egg and Milk. Implement a
-- type class Price, containing a function price. The price function
-- should return an Int representing the price of the item in a store.
--
-- The prices should be as follows:
-- * chicken eggs cost 20
-- * chocolate eggs cost 30
-- * milk costs 15 per liter

data Egg = ChickenEgg | ChocolateEgg
  deriving Show
data Milk = Milk Int -- amount in litres
  deriving Show


------------------------------------------------------------------------------
-- Ex 6: define the necessary instances in order to be able to compute these:
--
-- price [Just (ChocolateEgg), Nothing, Just (ChickenEgg)] ==> 50
-- price [Nothing, Nothing, Just (Milk 1), Just (Milk 2)]  ==> 45


------------------------------------------------------------------------------
-- Ex 7: below you'll find the datatype Number, which is either an
-- Integer, or a special value Infinite.
--
-- Implement an Ord instance so that finite Numbers compare normally,
-- and Infinite is larger than any other value.

data Number = Finite Integer | Infinite
  deriving (Show,Eq)


------------------------------------------------------------------------------
-- Ex 8: rational numbers have a numerator and a denominator that are
-- integers, usually separated by a horizontal bar or a slash:
--
--      numerator
--    -------------  ==  numerator / denominator
--     denominator
--
-- You may remember from school that two rationals a/b and c/d are
-- equal when a*d == b*c. Implement the Eq instance for rationals
-- using this definition.
--
-- You may assume in all exercises that the denominator is always
-- positive and nonzero.
--
-- Examples:
--   RationalNumber 4 5 == RationalNumber 4 5    ==> True
--   RationalNumber 12 15 == RationalNumber 4 5  ==> True
--   RationalNumber 13 15 == RationalNumber 4 5  ==> False

data RationalNumber = RationalNumber Integer Integer
  deriving Show

instance Eq RationalNumber where
  p == q = todo

------------------------------------------------------------------------------
-- Ex 9: the purpose of the simplfy function is to turn a rational number into
-- its simplest form, i.e.
--
--     ca         a
--    ----  ==>  ---.
--     cb         b
--
-- As a concrete example,
--
--     12        3 * 4         4
--    ----  ==  -------  ==>  ---.
--     15        3 * 5         5
--
-- Hint: Remember the function gcd?

simplify :: RationalNumber -> RationalNumber
simplify p = todo

------------------------------------------------------------------------------
-- Ex 10: implement the typeclass Num for RationalNumber. The results
-- of addition an multiplication must be simplified.
--
-- Examples:
--   RationalNumber 1 2 - RationalNumber 1 3 ==> RationalNumber 1 6
--   RationalNumber 1 3 * RationalNumber 3 1 ==> RationalNumber 1 1
--   fromInteger 17 :: RationalNumber        ==> RationalNumber 17 1
--   abs (RationalNumber (-3) 2)             ==> RationalNumber 3 2
--   signum (RationalNumber (-3) 2)          ==> RationalNumber (-1) 1
--   signum (RationalNumber 0 0)             ==> RationalNumber 0 0

instance Num RationalNumber where
  p + q = todo
  p * q = todo
  abs q = todo
  signum q = todo
  fromInteger x = todo
  negate q = todo
