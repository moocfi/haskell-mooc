{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Set6Test where

import Data.List
import Data.Maybe
import Data.Char (toUpper)
import Language.Haskell.TH.Syntax (lift,isInstance,mkName)
import Test.QuickCheck

import Mooc.Th
import Mooc.Test

import Set6

testEquals x y exp = $(testing [|x == y|]) (?==exp)

ex1 = conjoin [testEquals Finland Finland True
              ,testEquals Norway Norway True
              ,testEquals Switzerland Switzerland True
              ,testEquals Finland Norway False
              ,testEquals Finland Switzerland False
              ,testEquals Norway Finland False
              ,testEquals Norway Switzerland False
              ,testEquals Switzerland Finland False
              ,testEquals Switzerland Norway False]

ex2 = conjoin
  [Finland ?<= Finland, Finland ?<= Norway, Finland ?<= Switzerland
  ,Norway ?<= Norway, Norway ?<= Switzerland
  ,Switzerland ?<= Switzerland
  ,Norway ?> Finland
  ,Switzerland ?> Norway, Switzerland ?> Finland
  ,$(testing [|compare Norway Switzerland|]) (?==LT)
  ,$(testing [|compare Finland Finland|]) (?==EQ)
  ,$(testing [|min Switzerland Norway|]) (?==Norway)
  ,$(testing [|max Norway Finland|]) (?==Norway)]
  where x ?<= y = $(testing [|x <= y|]) (?==True)
        x ?>  y = $(testing [|x > y|]) (?==True)

word = listOf1 (choose ('a','z'))

capitalizeOne :: String -> Gen String
capitalizeOne [c] = return [toUpper c]
capitalizeOne (c:cs) = oneof [return (toUpper c:cs)
                             ,(c:) <$> capitalizeOne cs]

ex3 = property $ do
  name <- word
  diff <- word `suchThat` (/=name)
  namev1 <- capitalizeOne name
  namev2 <- capitalizeOne name
  diffv1 <- capitalizeOne diff
  return $ conjoin [testEquals (Name namev1) (Name namev2) True
                   ,testEquals (Name namev1) (Name diffv1) False
                   ,testEquals (Name namev1) (Name (namev1++diff)) False]

fromList = foldr LNode Empty

ex4_eq = forAll_ $ \(xs::[Bool]) -> testEquals (fromList xs) (fromList xs) True

ex4_neq = forAll_ $ \(xs::[Int],ys) -> testEquals (fromList xs) (fromList ys) (xs == ys)

ex5_egg = $(withInstance "Price" "Egg" [|price|]) $ \price -> conjoin [$(testing [|price ChocolateEgg|]) (?==30)
                                                                      ,$(testing [|price ChickenEgg|]) (?==20)]


ex5_milk = $(withInstance "Price" "Milk" [|price|]) $ \price ->
  forAllBlind (choose (0,100)) $ \l ->
  $(testing [|price (Milk l)|]) (?==(15*l))

ex6_maybe_egg =
  $(withInstanceType "Price" [t|Maybe Egg|] [|price|]) $ \price ->
  $(testing [|price [Just ChickenEgg]|]) (?==20)

ex6_maybe_milk =
  $(withInstanceType "Price" [t|Maybe Milk|] [|price|]) $ \price ->
  forAllBlind (choose (0,100)) $ \l ->
  $(testing [|price [Just (Milk l)]|]) (?==(15*l))

ex6_list_egg =
  $(withInstanceType "Price" [t|[Egg]|] [|price|]) $ \price ->
  $(testing [|price [ChocolateEgg, ChickenEgg]|]) (?==50)

ex6_list_milk =
  $(withInstanceType "Price" [t|[Milk]|] [|price|]) $ \price ->
  forAllBlind (choose (0,100)) $ \l ->
  $(testing [|price [Milk l, Milk 1]|]) (?==(15*(l+1)))

ex6_list_maybe_egg =
  $(withInstanceType "Price" [t|[Maybe Egg]|] [|price|]) $ \price ->
  conjoin [$(testing [|price [Just ChocolateEgg, Nothing, Just ChickenEgg]|]) (?==50)
          ,$(testing [|price [Nothing, Nothing, Nothing :: Maybe Egg]|]) (?==0)]


ex6_list_maybe_milk =
  $(withInstanceType "Price" [t|[Maybe Milk]|] [|price|]) $ \price ->
  forAllBlind (choose (0,10)) $ \milk1 ->
  forAllBlind (choose (0,10)) $ \milk2 ->
  conjoin [$(testing [|price [Just (Milk milk1), Nothing, Just (Milk milk2)]|]) (?==(15*milk1+15*milk2))
          ,$(testing [|price [Nothing, Nothing, Nothing :: Maybe Milk]|]) (?==0)]

instance Arbitrary Number where
  arbitrary = do
    b <- arbitrary
    if b then arbitrary >>= return . Finite else return Infinite

ex7_eq_check =
  $(withInstance "Eq" "Number" [|(==) :: Number -> Number -> Bool|]) $ \(==) ->
  forAll_ $ \i ->
  forAll_ $ \j ->
  conjoin [$(testing [|Finite i == Finite j|]) (?==(i Prelude.== j))
          ,$(testing [|Finite i == Infinite|]) (?==False)
          ,$(testing [|Infinite == Infinite|]) (?==True)]

ex7_finite_finite =
  $(withInstance "Ord" "Number" [|((<) :: Number -> Number -> Bool
                                  ,(>) :: Number -> Number -> Bool
                                  ,(<=) :: Number -> Number -> Bool
                                  ,(>=) :: Number -> Number -> Bool)|]) $ \((<),(>),(<=),(>=)) ->
  forAll_ $ \i ->
  forAll_ $ \(Positive delta) ->
  let a = Finite i
      b = Finite (i+delta)
  in conjoin [$(testing [|a < b|]) (?==True)
             ,$(testing [|b < a|]) (?==False)
             ,$(testing [|a > b|]) (?==False)
             ,$(testing [|b > a|]) (?==True)
             ,$(testing [|a <= b|]) (?==True)
             ,$(testing [|b <= a|]) (?==False)
             ,$(testing [|a >= b|]) (?==False)
             ,$(testing [|b >= a|]) (?==True)
             ]

ex7_finite_infinite =
  $(withInstance "Ord" "Number" [|((<) :: Number -> Number -> Bool
                                  ,(>) :: Number -> Number -> Bool
                                  ,(<=) :: Number -> Number -> Bool
                                  ,(>=) :: Number -> Number -> Bool)|]) $ \((<),(>),(<=),(>=)) ->
  forAll_ $ \i ->
  let a = Finite i
      b = Infinite
  in conjoin [$(testing [|a < b|]) (?==True)
             ,$(testing [|b < a|]) (?==False)
             ,$(testing [|a > b|]) (?==False)
             ,$(testing [|b > a|]) (?==True)
             ,$(testing [|a <= b|]) (?==True)
             ,$(testing [|b <= a|]) (?==False)
             ,$(testing [|a >= b|]) (?==False)
             ,$(testing [|b >= a|]) (?==True)
             ]

ex7_infinite_infinite =
  $(withInstance "Ord" "Number" [|((<) :: Number -> Number -> Bool
                                  ,(>) :: Number -> Number -> Bool
                                  ,(<=) :: Number -> Number -> Bool
                                  ,(>=) :: Number -> Number -> Bool)|]) $ \((<),(>),(<=),(>=)) ->
  conjoin [$(testing [|Infinite < Infinite|]) (?==False)
          ,$(testing [|Infinite > Infinite|]) (?==False)
          ,$(testing [|Infinite <= Infinite|]) (?==True)
          ,$(testing [|Infinite >= Infinite|]) (?==True)
          ]

ex7_max_finite =
  $(withInstance "Ord" "Number" [|max :: Number -> Number -> Number|]) $ \max ->
  forAll_ $ \i ->
  forAll_ $ \j ->
  $(testing [|max (Finite i) (Finite j)|]) (?==Finite (Prelude.max i j))

ex7_max_infinite =
  $(withInstance "Ord" "Number" [|max :: Number -> Number -> Number|]) $ \max ->
  forAll_ $ \i ->
  conjoin [$(testing [|max (Finite i) Infinite|]) (?==Infinite)
          ,$(testing [|max Infinite (Finite i)|]) (?==Infinite)
          ,$(testing [|max Infinite Infinite|]) (?==Infinite)]

instance Arbitrary RationalNumber where
  arbitrary = do num <- arbitrary
                 (Positive den) <- arbitrary
                 return (RationalNumber num den)

ex8_refl =
  $(withInstance "Eq" "RationalNumber" [|(==) :: RationalNumber -> RationalNumber -> Bool|]) $ \(==) ->
  forAll_ $ \r ->
  $(testing [|r == r|]) (?==True)

ex8_eq = $(withInstance "Eq" "RationalNumber" [|(==) :: RationalNumber -> RationalNumber -> Bool|]) $ \(==) ->
  forAll_ $ \r@(RationalNumber p q) ->
  forAll_ $ \(NonZero i) ->
  forAll_ $ \(NonZero j) ->
  conjoin [$(testing [|r == (RationalNumber (i*p) (i*q))|]) (?==True)
          ,$(testing [|r == (RationalNumber (i*p+j) (i*q))|]) (?==False)]

primes = nubBy (\x y -> mod y x == 0) [2..]
genPrime = elements $ take 100 primes

identical :: RationalNumber -> RationalNumber -> Bool
identical (RationalNumber a b) (RationalNumber c d) = a==c && b==d

exactly should was = expectation should was (identical should was)

genSimpl = do
  q <- genPrime
  p <- choose (1,q-1)
  return $ RationalNumber p q

ex9_prime = forAllBlind genSimpl $ \(RationalNumber p q) ->
  forAll_ $ \(Positive i) ->
  $(testing [|simplify (RationalNumber (i*p) (i*q))|]) (exactly (RationalNumber p q))

ex9_eq = forAll_ $ \r@(RationalNumber p q) ->
  forAll_ $ \(Positive i) ->
  let s = RationalNumber (i*p) (i*q)
  in $(testing [|simplify r|]) . was $ \r' ->
    $(testing [|simplify s|]) . was $ \s' ->
    counterexample "  Should be the same" (identical r' s')

ex10_add_zero = $(withInstance "Num" "RationalNumber" [|(+) :: RationalNumber -> RationalNumber -> RationalNumber|]) $ \(+) ->
  forAllBlind genSimpl $ \r ->
  forAll_ $ \(Positive q) ->
  $(testing [|r + (RationalNumber 0 q)|]) (exactly r)

ex10_add_commut = $(withInstance "Num" "RationalNumber" [|(+) :: RationalNumber -> RationalNumber -> RationalNumber|]) $ \(+) ->
  forAll_ $ \r ->
  forAll_ $ \s ->
  $(testing [|r + s|]) . was $ \was1 ->
  $(testing [|s + r|]) . was $ \was2 ->
  counterexample "  Should be the same" (identical was1 was2)

ex10_add_whole = $(withInstance "Num" "RationalNumber" [|(+) :: RationalNumber -> RationalNumber -> RationalNumber|]) $ \(+) ->
  forAll_ $ \(Positive q) ->
  forAll_ $ \(i,j) ->
  $(testing [|RationalNumber (q*i) q + RationalNumber (q*j) q|]) (exactly (RationalNumber (i Prelude.+ j) 1))

ex10_add = $(withInstance "Num" "RationalNumber" [|(+) :: RationalNumber -> RationalNumber -> RationalNumber|]) $ \(+) ->
  forAll_ $ \(Positive n) ->
  $(testing [|RationalNumber 1 (n Prelude.+ 1) + RationalNumber 1 (n*(n Prelude.+ 1))|]) (exactly (RationalNumber 1 n))

ex10_multiply_one = $(withInstance "Num" "RationalNumber" [|(*) :: RationalNumber -> RationalNumber -> RationalNumber|]) $ \(*) ->
  forAllBlind genSimpl $ \r ->
  forAll_ $ \(Positive q) ->
  $(testing [|r * (RationalNumber q q)|]) (exactly r)

ex10_multiply_commut = $(withInstance "Num" "RationalNumber" [|(*) :: RationalNumber -> RationalNumber -> RationalNumber|]) $ \(*) ->
  forAll_ $ \r ->
  forAll_ $ \s ->
  $(testing [|r * s|]) . was $ \was1 ->
  $(testing [|s * r|]) . was $ \was2 ->
  counterexample "  Should be the same" (identical was1 was2)

ex10_multiply_inverse = $(withInstance "Num" "RationalNumber" [|(*) :: RationalNumber -> RationalNumber -> RationalNumber|]) $ \(*) ->
  forAllBlind genPrime $ \p ->
  forAllBlind genPrime $ \q ->
  $(testing [|RationalNumber p q * RationalNumber q p|]) (exactly (RationalNumber 1 1))

ex10_multiply = $(withInstance "Num" "RationalNumber" [|(*) :: RationalNumber -> RationalNumber -> RationalNumber|]) $ \(*) ->
  forAllBlind genPrime $ \p ->
  forAllBlind genPrime $ \q ->
  forAllBlind genPrime $ \r ->
  nub [p,q,r] == [p,q,r] ==>
  forAll_ $ \(Positive i) ->
  $(testing [|RationalNumber (i Prelude.* p) q * RationalNumber q (r Prelude.* i)|]) (exactly (RationalNumber p r))

ex10_abs = $(withInstance "Num" "RationalNumber" [|abs :: RationalNumber -> RationalNumber|]) $ \abs ->
  forAllBlind genSimpl $ \r@(RationalNumber p q) ->
  conjoin [$(testing [|abs (RationalNumber (-p) q)|]) (exactly r)
          ,$(testing [|abs r|]) (exactly r)]

ex10_signum_abs = $(withInstance "Num" "RationalNumber" [|abs :: RationalNumber -> RationalNumber|]) $ \abs ->
  forAll_ $ \r ->
  counterexample "signum r * abs r == r" $
  counterexample ("  failed with r = " ++ show r) $
  signum r * abs r == r

ex10_fromInteger = $(withInstance "Num" "RationalNumber" [|fromInteger :: Integer -> RationalNumber|]) $ \fromInteger ->
  forAll_ $ \(i,j) ->
  counterexample ("fromIntegral "++show i++" + fromIntegral "++show j) $
  exactly (RationalNumber (i+j) 1) (fromIntegral i + fromIntegral j)

ex10_negate = $(withInstance "Num" "RationalNumber" [|negate :: RationalNumber -> RationalNumber|]) $ \negate ->
  forAll_ $ \r ->
  counterexample ("("++show r++") + negate ("++show r++")") $
  exactly (RationalNumber 0 1) (r + negate r)

ex11_integer = $(withInstance "Addable" "Integer" [|(zero::Integer,add::Integer->Integer->Integer)|]) $ \(zero,add) ->
  forAll_ $ \(x::Integer,y) ->
  conjoin [$(testing [|add x y|]) (?==x+y)
          ,$(testing [|add zero x|]) (?==x)]

ex11_list = $(withInstance1 "Addable" "[]" [|(zero::[Bool],add::[Bool]->[Bool]->[Bool])|]) $ \(zero,add) ->
  forAll_ $ \(x::[Bool],y) ->
  conjoin [$(testing [|add x y|]) (?==x++y)
          ,$(testing [|add zero x|]) (?==x)
          ,$(testing [|add x zero|]) (?==x)]

ex12_step_color = $(withInstance "Cycle" "Color" [|step|]) $ \step ->
  conjoin [$(testing [|step Red|]) (?==Green)
          ,$(testing [|step Green|]) (?==Blue)
          ,$(testing [|step Blue|]) (?==Red)]

ex12_step_suit = $(withInstance "Cycle" "Suit" [|step|]) $ \step ->
  conjoin [$(testing [|step Club|]) (?==Spade)
          ,$(testing [|step Spade|]) (?==Diamond)
          ,$(testing [|step Diamond|]) (?==Heart)
          ,$(testing [|step Heart|]) (?==Club)]

ex12_stepMany_color = $(withInstance "Cycle" "Color" [|stepMany|]) $ \stepMany ->
  property $ do
    let vals = [Red,Green,Blue]
    i <- choose (0,2)
    j <- choose (0,20)
    return $ $(testing [|stepMany j (vals!!i)|]) (?==vals!!(mod (i+j) 3))

ex12_stepMany_suit = $(withInstance "Cycle" "Suit" [|stepMany|]) $ \stepMany ->
  property $ do
    let vals = [Club,Spade,Diamond,Heart]
    i <- choose (0,3)
    j <- choose (0,20)
    return $ $(testing [|stepMany j (vals!!i)|]) (?==vals!!(mod (i+j) 4))

ex12_stepMany_class = $(classContains "Cycle" "stepMany")

$(defineInstance "Cycle" ''Int "step" [|succ|])

$(return []) -- hack to make next test see instance

ex12_stepMany_default = counterexample "After defining\n  instance Cycle Int where step = succ" $
  $(withInstance "Cycle" "Int" [|stepMany|]) $ \stepMany ->
  forAllBlind (choose (0,100::Int)) $ \n ->
  forAllBlind (choose (0,100::Int)) $ \m ->
  $(testing [|stepMany n m|]) (?==n+m)

-- -- -- -- -- -- -- -- tests need to be defined after TH splice above

main = score tests

tests = [(1,"Eq Country",[ex1])
        ,(2,"Ord Country",[ex2])
        ,(3,"Eq Name",[ex3])
        ,(4,"Eq List",[ex4_eq, ex4_neq])
        ,(5,"Price",[ex5_egg, ex5_milk])
        ,(6,"Price List",[ ex6_maybe_egg, ex6_maybe_milk
                         , ex6_list_egg, ex6_list_milk
                         , ex6_list_maybe_egg, ex6_list_maybe_milk])
        ,(7,"Ord Number",[ ex7_eq_check
                         , ex7_finite_finite, ex7_finite_infinite, ex7_infinite_infinite
                         , ex7_max_finite, ex7_max_infinite ])
        ,(8,"Eq RationalNumber",[ex8_refl, ex8_eq])
        ,(9,"simplify",[ex9_prime, ex9_eq])
        ,(10,"Num RationalNumber",[ex10_add_zero, ex10_add_commut, ex10_add_whole, ex10_add
                                  ,ex10_multiply_one, ex10_multiply_commut, ex10_multiply_inverse, ex10_multiply
                                  ,ex10_abs
                                  ,ex10_signum_abs
                                  ,ex10_fromInteger
                                  ,ex10_negate
                                  ])
        ,(11,"Addable",[ex11_integer, ex11_list])
        ,(12,"Cycle",[ex12_step_color, ex12_step_suit, ex12_stepMany_color, ex12_stepMany_suit
                     ,ex12_stepMany_class, ex12_stepMany_default])
        ]
