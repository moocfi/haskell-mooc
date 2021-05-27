module Examples.Validation (Validation,invalid,check) where

import Control.Applicative
import Data.Char (isDigit)

data Validation a = Ok a | Errors [String]
  deriving (Show,Eq)

instance Functor Validation where
  fmap f (Ok x) = Ok (f x)
  fmap _ (Errors e) = Errors e

instance Applicative Validation where
  pure x = Ok x
  liftA2 f (Ok x)      (Ok y)      = Ok (f x y)
  liftA2 f (Errors e1) (Ok y)      = Errors e1
  liftA2 f (Ok x)      (Errors e2) = Errors e2
  liftA2 f (Errors e1) (Errors e2) = Errors (e1++e2)

invalid :: String -> Validation a
invalid err = Errors [err]

check :: Bool -> String -> a -> Validation a
check b err x
  | b = pure x
  | otherwise = invalid err

----
---- Example
----

birthday :: String -> Int -> Validation String
birthday name age = liftA2 congratulate checkedName checkedAge
  where checkedName = check (length name < 10) "Name too long" name
        checkedAge = check (age < 99) "Too old" age
        congratulate n a = "Happy "++show a++"th birthday "++n++"!"

-- birthday "Guy" 31
--   ==> Ok "Happy 31th birthday Guy!"
-- birthday "Guybrush Threepwood" 31
--   ==> Errors ["Name too long"]
-- birthday "Yog-sothoth" 10000
--   ==> Errors ["Name too long","Too old"]

----
---- Example: validating lists
----

-- via recursion
allPositive :: [Int] -> Validation [Int]
allPositive [] = Ok []
allPositive (x:xs) = liftA2 (:) checkThis checkRest
  where checkThis = check (x>=0) ("Not positive: "++show x) x
        checkRest = allPositive xs

-- allPositive [1,2,3] ==>  Ok [1,2,3]
-- allPositive [1,2,3,-4] ==> Errors ["Not positive: -4"]
-- allPositive [1,-2,3,-4] ==> Errors ["Not positive: -2","Not positive: -4"]

-- via traverse
allPositive' :: [Int] -> Validation [Int]
allPositive' xs = traverse checkNumber xs
  where checkNumber x = check (x>=0) ("Not positive: "++show x) x

---
--- Alternative instance
---

instance Alternative Validation where
  empty = Errors []
  Ok x <|> _ = Ok x
  Errors e1 <|> Ok y = Ok y
  Errors e1 <|> Errors e2 = Errors (e1++e2)

----
---- Example: parsing contact information:
----

data ContactInfo = Email String | Phone String
  deriving Show

validateEmail :: String -> Validation ContactInfo
validateEmail s = check (elem '@' s) "Not an email: should contain a @" (Email s)

checkLength :: String -> Validation ContactInfo
checkLength s = check (length s <= 10) "Not a phone number: should be at most 10 digits" (Phone s)

checkDigits :: String -> Validation ContactInfo
checkDigits s = check (all isDigit s) "Not a phone number: should be all numbers" (Phone s)

validatePhone :: String -> Validation ContactInfo
validatePhone s = checkDigits s *> checkLength s

validateContactInfo :: String -> Validation ContactInfo
validateContactInfo s = validateEmail s <|> validatePhone s

-- validateContactInfo "user@example.com"
--   ==> Ok (Email "user@example.com")
-- validateContactInfo "01234"
--   ==> Ok (Phone "01234")
-- validateContactInfo "01234567890"
--   ==> Errors ["Not an email: should contain a @","Not a phone number: should be at most 10 digits"]
-- validateContactInfo "01234567890x"
--   ==> Errors ["Not an email: should contain a @",
--               "Not a phone number: should be all numbers",
--               "Not a phone number: should be at most 10 digits"]
-- validateContactInfo "x"
--   ==> Errors ["Not an email: should contain a @",
--               "Not a phone number: should be all numbers"]
