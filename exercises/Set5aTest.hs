{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Set5aTest where

import Data.List
import Test.QuickCheck

import Mooc.Th
import Mooc.Test

import Set5a

main = score tests

tests = [(1,"Vehicle",[ex1])
        ,(2,"BusTicket",[ex2])
        ,(3,"ShippingEntry",[ex3_totalPrice, ex3_buyOneMore])
        ,(4,"Person",[ex4_name, ex4_age])
        ,(5,"Position",[ex5_simple, ex5_generate])
        ,(6,"Student",[ex6_freshman, ex6_nth, ex6_graduated, ex6_graduate])
        ,(7,"UpDown",[ex7_type, ex7_simple, ex7_generate])
        ,(8,"Color",[ex8_red, ex8_green, ex8_blue, ex8_simple, ex8_complicated])
        ,(9,"OneOrTwo",[ex9])
        ,(10,"KeyVals",[ex10_type, ex10_string_bool, ex10_int_int])
        ,(11,"Nat",[ex11_to, ex11_from, ex11_just, ex11_nothing])
        ,(12,"Bin",[ ex12_prettyPrint
                   , ex12_from_zero, ex12_to_zero, ex12_to_from_zero
                   , ex12_from_to_zero, ex12_from_one, ex12_to_one
                   , ex12_to_from_one, ex12_from_to_one, ex12_to_from
                   , ex12_from_to])
        ]

-- -- -- -- -- -- -- --

assertNormalConstructor (Constructor _ _) = property True
assertNormalConstructor (Weird n) = counterexample ("  constructor "++n++" is not a normal constructor. Perhaps it's a record constructor or an infix constructor? Make it normal.") False

assertNoFields (Constructor n vs) = counterexample ("  constructor "++n++" should have no fields") $ null vs
assertNoFields _ = property True

assertFields exp (Constructor n vs) = counterexample ("  constructor "++n++" field types") $ vs ?== exp
assertFields _ _ = property True

ex1 = $(reifyType "Vehicle") $ \(DataType vars cs) ->
  conjoin [counterexample "  should have no type parameters" $ vars == []
          ,counterexample "  should have 4 constructors" $ length cs ?== 4
          ,conjoin $ map assertNormalConstructor cs
          ,conjoin $ map assertNoFields cs
          ,counterexample "  constructor names" $
           hasElements ["Bike","Bus","Train","Tram"] [n | Constructor n _ <- cs]
          ]

ex2 = $(reifyType "BusTicket") $ \(DataType vars cs) ->
  conjoin [counterexample "  should have no type parameters" $ vars == []
          ,counterexample "  should have 2 constructors" $ length cs ?== 2
          ,counterexample "  constructor names" $
            hasElements ["SingleTicket", "MonthlyTicket"] [n | Constructor n _ <- cs]
          ,conjoin $ map assertNormalConstructor cs
          ,counterexample "  should have a constructor SingleTicket with no fields" $
           any (==Constructor "SingleTicket" []) cs
          ,counterexample "  should have a constructor MonthlyTicket with a String field" $
           counterexample (show cs) $
           any (==Constructor "MonthlyTicket" [SimpleType "String"]) cs
           || any (==Constructor "MonthlyTicket" [SimpleType "[Char]"]) cs
          ]

word = listOf1 (choose ('a','z'))

ex3_totalPrice = forAllBlind word $ \name ->
  forAll_ $ \count ->
  forAllBlind (elements [0.1,0.25..2.0]) $ \price ->
  $(testing [|totalPrice (MkShoppingEntry name price count)|]) (?~=price * fromIntegral count)

eq (MkShoppingEntry n p c) (MkShoppingEntry n' p' c') = and [n==n',p==p',c==c']

ex3_buyOneMore = forAllBlind word $ \name -> forAll_ $ \(price,count) ->
  let exp = MkShoppingEntry  name price (succ count)
  in $(testing [|buyOneMore (MkShoppingEntry name price count)|]) (\v -> expectation exp v (eq exp v))

ex4_name = property $ do
  n <- word
  return $ counterexample ("getName (setName "++show n++" fred)") $
    getName (setName n fred) === n

ex4_age = property $ do
  a <- choose (0,89)
  return $ counterexample ("getAge (setAge "++show a++" fred)") $
    getAge (setAge a fred) === a

ex5_simple = conjoin [$(testing' [|getX origin|]) (?==0)
                     ,$(testing' [|getY origin|]) (?==0)
                     ,$(testing' [|getX (right origin)|]) (?==1)
                     ,$(testing' [|getY (up origin)|]) (?==1)]

ex5_generate = forAllShrink_ (choose (0,20)) $ \a ->
  forAllShrink_ (choose (0,20)) $ \b' ->
  let b = a+b'
      pos0 = iterate (right . up) origin !! a
      pos1 = iterate up pos0 !! b'
  in counterexample ("After starting from origin and going right "++show a++" times and up "++show b++" times:") $
    conjoin [counterexample "getX" (getX pos1 ?== a)
            ,counterexample "getY" (getY pos1 ?== b)]

ex6_freshman = $(testing [|study Freshman|]) (?==NthYear 1)
ex6_nth = forAllBlind (choose (1,6)) $ \i -> $(testing [|study (NthYear i)|]) (?==NthYear (i+1))
ex6_graduate = $(testing [|study (NthYear 7)|]) (?==Graduated)
ex6_graduated = $(testing [|study Graduated|]) (?==Graduated)

ex7_type = $(reifyType "UpDown") $ \(DataType vars cs) ->
  conjoin [counterexample "  should have no type parameters" $ vars == []
          ,counterexample "  should have 2 constructors" $ length cs ?== 2
          ,conjoin $ map (assertFields [SimpleType "Int"]) cs
          ]

ex7_simple = conjoin [$(testing' [|get zero|]) (?== 0)
                     ,$(testing' [|get (tick zero)|]) (?== 1)
                     ,$(testing' [|get (tick (toggle zero))|]) (?== negate 1)
                     ,$(testing' [|get (toggle (tick (zero)))|]) (?== 1)]

ex7_generate =
  forAllShrink_ (choose (0,20)) $ \a ->
  forAllShrink_ (choose (0,20)) $ \b ->
  let tc0 = iterate tick zero !! a
      tc1 = iterate tick (toggle tc0) !! b
  in counterexample ("Did "++show a++" ticks, a toggle, and "++show b++" ticks:") $
     counterexample "get:" (get tc1 ?== a-b)

ex8_red = $(testing [|rgb Red|]) (?==[1,0,0])
ex8_green = $(testing [|rgb Green|]) (?==[0,1,0])
ex8_blue = $(testing [|rgb Blue|]) (?==[0,0,1])

fcmp expected actual =
  expectation expected actual (length expected == length actual && diff < eps)
  where diff = sum . map abs $ zipWith (-) actual expected
        eps = 0.01

ex8_simple = property $ do
  ~[c1,c2,c3,c4] <- shuffle [Red,Red,Red,Green]
  return $ conjoin [$(testing [|rgb (Mix (Mix c1 c2) (Mix c3 c4))|]) (fcmp [0.75,0.25,0])
                   ,$(testing [|rgb (Invert (Mix (Mix c1 c2) (Mix c3 c4)))|]) (fcmp [0.25,0.75,1.0])]

ex8_complicated = property $ do
  cs <- vectorOf 4 (elements [Red,Invert Red,Green,Invert Green,Blue,Invert Blue])
  let ~[c1,c2,c3,c4] = cs
      r = fromIntegral $ length $ filter (has Red) cs
      g = fromIntegral $ length $ filter (has Green) cs
      b = fromIntegral $ length $ filter (has Blue) cs
  return $ $(testing [|rgb (Mix (Mix c1 c2) (Mix c3 c4))|]) (fcmp [r/4, g/4, b/4])
  where has Red Red = True
        has Blue Blue = True
        has Green Green = True
        has x (Invert c) = not (has x c)
        has _ _ = False

ex9 = $(reifyType "OneOrTwo") $ \(DataType vars cs) ->
  conjoin [counterexample "  should have one type parameter" $ length vars ?== 1
          ,counterexample "  should have 2 constructors" $ length cs ?== 2
          ,counterexample "  constructor names" $
            hasElements ["One", "Two"] [n | Constructor n _ <- cs]
          ,conjoin $ map assertNormalConstructor cs
          ,counterexample "  should have a constructor One with one field of the parameter type" $
           any ((==) $ Constructor "One" $ map SimpleType vars) cs
          ,counterexample "  should have a constructor Two with two fields of the parameter type" $
           any ((==) $ Constructor "Two" $ map SimpleType $ vars++vars) cs
          ]

ex10_type = $(reifyType "KeyVals") $ \(DataType vars cs) ->
  conjoin [counterexample "  type parameters" $ ["k","v"] ?== vars
          ,counterexample "  should have 2 constructors" $ length cs ?== 2
          ,conjoin $ map assertNormalConstructor cs
          ,counterexample "  constructor names" $
           hasElements ["Empty", "Pair"] [n | Constructor n _ <- cs]
          ,counterexample "  should have a constructor Empty with no fields" $
            any (==Constructor "Empty" []) cs
          ,counterexample "  should have a constructor Pair with the right fields" $
            any (==Constructor "Pair" [SimpleType "k",SimpleType "v",SimpleType "KeyVals k v"]) cs
          ]

m_ex10 vs = counterexample ("toList (fromList "++show vs++"))") $ vs ==? toList (fromList vs)

ex10_string_bool = forAll_ $ \(kvs::[(String,Bool)]) -> m_ex10 kvs

ex10_int_int = forAll_ $ \(kvs::[(Int,Int)]) -> m_ex10 kvs

instance Arbitrary Nat where
  arbitrary = oneof $ map return $ take 20 $ iterate PlusOne Zero

ex11_to   = $(testing [| toNat 0 |]) (?== Just Zero)
ex11_from = $(testing [| fromNat Zero |]) (?== 0)
ex11_just = forAll_ $ \(n :: Nat) ->
  counterexample ("  toNat (fromNat " ++ show n ++ ")")
  (toNat (fromNat n) ?== Just n)
ex11_nothing = forAll_ $ \(Negative (z :: Int)) ->
  $(testing [|toNat z|]) (?== Nothing)

instance Arbitrary Bin where
  arbitrary = do
    bs <- arbitrary :: Gen [Bool]
    return $ foldr (\x b -> if x then I b else O b) (I End) (take 8 bs)

ex12_prettyPrint =
  forAllBlind (listOf1 (elements "01")) $ \w->
  let inp = foldl' (\acc i -> case i of '0' -> O acc; '1' -> I acc; _ -> error (show i)) End w
  in $(testing [|prettyPrint inp|]) (?==w)

ex12_from_zero    =
  counterexample ("fromBin (O End)") (fromBin (O End) ?== 0)
ex12_to_zero      =
  counterexample ("toBin 0") (toBin 0 ?== O End)
ex12_to_from_zero =
  counterexample ("toBin (fromBin (O End))") (toBin (fromBin (O End)) ?== O End)
ex12_from_to_zero =
  counterexample ("fromBin (toBin 0)") (fromBin (toBin 0) ?== 0)
ex12_from_one    =
  counterexample ("fromBin (I End)") (fromBin (I End) ?== 1)
ex12_to_one      =
  counterexample ("toBin 1") (toBin 1 ?== I End)
ex12_to_from_one =
  counterexample ("toBin (fromBin (I End))") (toBin (fromBin (I End)) ?== I End)
ex12_from_to_one =
  counterexample ("fromBin (toBin 1)") (fromBin (toBin 1) ?== 1)
ex12_to_from      = forAll_ $ \(b :: Bin) ->
  counterexample ("toBin (fromBin (" ++ show b ++ "))") (toBin (fromBin b) ?== b)
ex12_from_to      = forAll_ $ \(n :: Int) -> n < 0 .||.
  counterexample ("fromBin (toBin (" ++ show n ++ "))") (fromBin (toBin n) ?== n)
