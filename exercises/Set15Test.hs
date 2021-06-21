{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, StandaloneDeriving, FlexibleInstances #-}

module Set15Test where

import Mooc.Test
import Mooc.Th
import Examples.Validation

import Control.Applicative

import Test.QuickCheck hiding (Result,Fun,Failure)

import Set15

main = score tests

tests = [(1,"sumTwoMaybes",[ex1])
        ,(2,"statements",[ex2_1, ex2_2])
        ,(3,"calculator",[ex3_success, ex3_failure_1, ex3_failure_2])
        ,(4,"validateDiv",[ex4])
        ,(5,"validateAddress",[ex5_ok, ex5_fail])
        ,(6,"twoPersons",[ex6_maybe,ex6_list])
        ,(7,"boolOrInt",[ex7_ok, ex7_fail])
        ,(8,"normalizePhone",[ex8])
        ,(9,"parseExpression",[ex9_ok, ex9_fail])
        ,(10,"Priced",[ex10_fmap, ex10_liftA2])
        ,(11,"<*> via liftA2",[ex11])
        ,(12,"fmap via liftA2",[ex12])
        ,(13,"tryAll",[ex13_maybe, ex13_list, ex13_validation])
        ,(14,"Functor Both",[ex14_ok, ex14_fail])
        ,(15,"Applicative Both",[ex15_pure, ex15_liftA2, ex15_liftA2_2])
        ]

-- -- -- -- --

ex1 = forAll_ $ \i ->
  forAll_ $ \j ->
  conjoin [$(testing [|sumTwoMaybes (Just i) (Just j)|]) (?==Just (i+j))
          ,$(testing [|sumTwoMaybes (Just i) n|]) (?==Nothing)
          ,$(testing [|sumTwoMaybes n (Just j)|]) (?==Nothing)
          ,$(testing [|sumTwoMaybes n n|]) (?==Nothing)]
  where n = Nothing :: Maybe Int

ex2_1 = conjoin
  [$(testing [|statements ["x"] ["y"]|]) (hasElements ["x is y","x is not y"])
  ,$(testing [|statements ["x"] ["y","w"]|]) (hasElements ["x is y","x is w","x is not y","x is not w"])
  ,$(testing [|statements ["x","z"] ["y","w"]|]) (hasElements ["x is y","x is w","x is not y","x is not w"
                                                              ,"z is y","z is w","z is not y","z is not w"])
  ,$(testing [|statements ([]::[String]) ["y","w"]|]) (?== [])]

word = listOf1 (choose ('a','z'))

ex2_2 = forAllBlind (resize 8 $ listOf1 word) $ \xs ->
  forAllBlind (resize 8 $ listOf1 word) $ \ys ->
  forAllBlind (elements xs) $ \x ->
  forAllBlind (elements ys) $ \y ->
  forAllBlind (elements [" is "," is not "]) $ \i ->
  let exp = x ++ i ++ y
  in $(testing [|statements xs ys|]) . was $ \out ->
      counterexample ("  Should contain "++show exp) $
      elem exp out

ex3_success = forAllBlind (choose (-100,100)) $ \i ->
  let s = show i
  in conjoin [$(testing [|calculator "negate" s|]) (?==Just (negate i))
             ,$(testing [|calculator "double" s|]) (?==Just (2*i))]

ex3_failure_1 = forAllBlind word $ \w ->
  forAllBlind (choose (-100,100::Int)) $ \i ->
  not (elem w ["negate","double"]) ==> $(testing [|calculator w (show i)|]) (?==Nothing)

ex3_failure_2 = forAll (elements ["negate","double","xyzzy"]) $ \w ->
  forAllBlind (choose (-100,100::Int)) $ \i ->
  let s = show i
  in conjoin [$(testing [|calculator w ""|]) (?==Nothing)
             ,$(testing [|calculator w (s++".3")|]) (?==Nothing)
             ,forAllBlind (shuffle (s++"z")) $ \s' -> $(testing [|calculator w s'|]) (?==Nothing)]

ok :: a -> Validation a
ok = pure

invalids :: [String] -> Validation a
invalids = foldr1 (<|>) . map invalid

ex4 = forAllBlind (choose (0,100)) $ \i ->
  forAllBlind (choose (1,10)) $ \j ->
  conjoin [$(testing [|validateDiv i j|]) (?==ok (div i j))
          ,$(testing [|validateDiv i 0|]) (?==invalid "Division by zero!")]

postCode = vectorOf 5 (choose ('0','9'))
street = listOf1 (elements (' ':['a'..'z'])) `suchThat` ((<=20).length)

ex5_ok = forAllBlind postCode $ \p ->
  forAllBlind street $ \s ->
  forAllBlind (fmap show (choose (1,1000::Int))) $ \n ->
  $(testing [|validateAddress s n p|]) (?==ok (Address s n p))

ex5_fail = forAllBlind postCode $ \p ->
  forAllBlind street $ \s ->
  forAllBlind (fmap show (choose (1,1000::Int))) $ \n ->
  forAllBlind (choose ('a','z')) $ \chaff ->
  forAllBlind (vectorOf 20 (choose ('a','z'))) $ \pad ->
  forAllBlind (shuffle (chaff:take 4 p)) $ \p' ->
  forAllBlind (shuffle (chaff:n)) $ \n' ->
  conjoin [$(testing [|validateAddress (s++pad) n p|]) (?==invalid "Invalid street name")
          ,$(testing [|validateAddress s n (take 4 p)|]) (?==invalid "Invalid postcode")
          ,$(testing [|validateAddress s n p'|]) (?==invalid "Invalid postcode")
          ,$(testing [|validateAddress s n' p|]) (?==invalid "Invalid street number")
          ,$(testing [|validateAddress (s++pad) n p'|]) (?==invalids ["Invalid street name", "Invalid postcode"])
          ,$(testing [|validateAddress (s++pad) n' (take 4 p)|]) (?==invalids ["Invalid street name", "Invalid street number","Invalid postcode"])]

ex6_maybe = forAllBlind word $ \name1 ->
  forAllBlind word $ \name2 ->
  forAllBlind (choose (10,100)) $ \age1 ->
  forAllBlind (choose (10,100)) $ \age2 ->
  forAll_ $ \(emp1,emp2) ->
  conjoin [$(testing [|twoPersons (Just name1) (Just age1) (Just emp1) (Just name2) (Just age2) (Just emp2)|])
           (?==Just [Person name1 age1 emp1, Person name2 age2 emp2])
          ,$(testing [|twoPersons (Just name1) (Nothing::Maybe Int) (Just emp1) (Just name2) (Just age2) (Just emp2)|])
           (?==Nothing)
          ,$(testing [|twoPersons (Just name1) (Just age1) (Just emp1) (Just name2) (Just age2) (Nothing::Maybe Bool)|])
           (?==Nothing)]

ex6_list = forAllBlind word $ \name1 ->
  forAllBlind word $ \name2 ->
  forAllBlind word $ \name3 ->
  forAllBlind (choose (10,100)) $ \age1 ->
  forAllBlind (choose (10,100)) $ \age2 ->
  forAllBlind (choose (10,100)) $ \age3 ->
  forAll_ $ \(emp1,emp2) ->
  conjoin [$(testing [|twoPersons [name1] [age1] [emp1] [name2] [age2] [emp2]|])
           (?==[[Person name1 age1 emp1, Person name2 age2 emp2]])
          ,$(testing [|twoPersons [name1,name3] [age1] [emp1] [name2] [age2] [emp2]|])
           (?==[[Person name1 age1 emp1, Person name2 age2 emp2]
               ,[Person name3 age1 emp1, Person name2 age2 emp2]])
          ,$(testing [|twoPersons [name1] [age1] [emp1] [name2] [age2,age3] [emp2]|])
           (?==[[Person name1 age1 emp1, Person name2 age2 emp2]
               ,[Person name1 age1 emp1, Person name2 age3 emp2]])]

ex7_ok = forAll_ $ \(i::Int) ->
  forAll_ $ \(b::Bool) ->
  conjoin [$(testing [|boolOrInt (show i)|]) (?==ok (Right i))
          ,$(testing [|boolOrInt (show b)|]) (?==ok (Left b))]

sow :: [a] -> [a] -> Gen [a]
sow xs [] = return xs
sow [] ys = return ys
sow (x:xs) (y:ys) = oneof [fmap (x:) $ sow xs (y:ys)
                          ,fmap (y:) $ sow (x:xs) ys]

ex7_fail = forAll_ $ \(i::Int) ->
  forAllBlind (choose ('a','n')) $ \c -> -- don't generate 0o or 0x by accident
  forAllBlind (shuffle (c:show i)) $ \ni ->
  forAll_ $ \(b::Bool) ->
  forAllBlind (sow [c] (show b)) $ \nb ->
  conjoin [$(testing [|boolOrInt ni|]) (?==invalids ["Not a Bool","Not an Int"])
          ,$(testing [|boolOrInt nb|]) (?==invalids ["Not a Bool","Not an Int"])]

digit = choose ('0','9')

ex8 = forAllBlind (take 10 <$> listOf1 digit) $ \out ->
  forAllBlind (listOf1 (return ' ')) $ \spaces ->
  forAllBlind word $ \chaff ->
  forAllBlind (sow out spaces) $ \inp ->
  forAllBlind (sow inp chaff) $ \wrong ->
  conjoin [$(testing [|normalizePhone inp|]) (?==ok out)
          ,$(testing [|normalizePhone (inp++"9999999999")|]) (?==invalid "Too long")
          ,$(testing [|normalizePhone wrong|])
           (?==invalids ((if length out + length chaff > 10 then ["Too long"] else [])++["Invalid character: "++[d] | d <- chaff]))]

showE (Plus x y) = showA x ++ " + " ++showA y
showE (Minus x y) = showA x ++ " - " ++showA y

showA (Number i) = show i
showA (Variable v) = [v]

genA = oneof [Number <$> choose (0,10)
             ,Variable <$> choose ('a','z')]

genE = elements [Plus,Minus] <*> genA <*> genA

ex9_ok = forAllBlind genE $ \exp ->
  let s = showE exp
  in $(testing [|parseExpression s|]) (?==ok exp)

ex9_fail = forAllBlind (showA<$>genA) $ \goodArg ->
  forAllBlind (elements ["1z","1.0","xy","."]) $ \badArg ->
  forAllBlind (elements ["+","-"]) $ \goodOp ->
  forAllBlind (elements ["*","%","x","3"]) $ \badOp ->
  let b x op y = x ++ " " ++ op ++ " " ++ y
      badExp1 = (goodArg ++ " " ++ goodOp)
      badExp2 = b goodArg goodOp goodArg ++ " " ++ goodOp
  in conjoin [$(testing [|parseExpression (b goodArg goodOp badArg)|])
               (?==invalids ["Invalid number: "++badArg, "Invalid variable: "++badArg])
             ,$(testing [|parseExpression (b badArg goodOp goodArg)|])
               (?==invalids ["Invalid number: "++badArg, "Invalid variable: "++badArg])
             ,$(testing [|parseExpression (b goodArg badOp goodArg)|])
               (?==invalid ("Unknown operator: "++badOp))
             ,$(testing [|parseExpression badExp1|])
               (?==invalid ("Invalid expression: "++badExp1))
             ,$(testing [|parseExpression badExp2|])
               (?==invalid ("Invalid expression: "++badExp2))
             ,$(testing [|parseExpression (b goodArg badOp badArg)|])
               (?==invalids ["Unknown operator: "++badOp,"Invalid number: "++badArg, "Invalid variable: "++badArg])
             ,$(testing [|parseExpression (b goodArg badOp badArg)|])
               (?==invalids ["Unknown operator: "++badOp,"Invalid number: "++badArg, "Invalid variable: "++badArg])]

ex10_fmap = conjoin [$(testing' [|fmap reverse (Priced 3 "abc")|]) (?==Priced 3 "cba")
                    ,$(testing' [|fmap negate (Priced 1 7)|]) (?==Priced 1 (-7))]

ex10_liftA2 = conjoin [$(testing' [|liftA2 (++) (Priced 3 "abc") (Priced 5 "de")|]) (?==Priced 8 "abcde")
                      ,$(testing' [|liftA2 (+) (pure 4) (Priced 3 6)|]) (?==Priced 3 10)]

deriving instance (Eq (f (g a))) => Eq (Both f g a)

ex11 = conjoin [$(testing' [|Just succ <#> Just 2|]) (?==Just 3)
               ,$(testing' [|Nothing <#> Just 2|]) (?==(Nothing :: Maybe Int))
               ,$(testing' [|Just reverse <#> Nothing|]) (?==(Nothing :: Maybe [Bool]))
               ,$(testing' [|[reverse,drop 1] <#> ["foo","bar"]|]) (?==["oof","rab","oo","ar"])]

ex12 = conjoin [$(testing' [|myFmap negate (Just 1)|]) (?==Just (-1))
               ,$(testing' [|myFmap negate Nothing|]) (?==Nothing)
               ,$(testing' [|myFmap negate [1,2]|]) (?==[-1,-2])
               ,$(testing' [|myFmap not [True,True,False]|]) (?==[False,False,True])
               ,$(testing' [|myFmap not Nothing|]) (?==Nothing)]

ex13_maybe = conjoin [$(testing' [|tryAll id [Nothing, Just 1, Just 2]|]) (?==Just 1)
                     ,$(testing' [|tryAll id [Nothing, Nothing]|]) (?==(Nothing::Maybe Int))
                     ,$(testing' [|tryAll id [] :: Maybe Int|]) (?==Nothing)]

ex13_list = conjoin [$(testing' [|tryAll tail ["abc", "d", "ef"]|]) (?=="bcf")
                    ,$(testing' [|tryAll tail []|]) (?==([]::String))]

tA l xs k = counterexample' ("tryAll (\\x -> if x>" ++ show l ++ " then pure x else invalid \"zero\") "++show xs) $
  k $ tryAll (\x -> if x>l then pure x else invalid "zero") xs

ex13_validation = forAllBlind (choose (0::Int,10)) $ \l ->
  forAllBlind (listOf1 (choose (0::Int,10))) $ \xs ->
  tA l xs (?==case filter (>l) xs of [] -> invalids (map (const "zero") xs)
                                     (x:_) -> ok x)

ex14_ok = conjoin [$(testing' [|fmap not (Both (Just [True]))|]) (?== Both (Just [False]))
                  ,$(testing' [|fmap negate (Both [Just 3, Just 4])|]) (?== Both [Just (-3), Just (-4)])
                  ,$(testing' [|fmap negate (Both [[1,2,3],[4,5]])|]) (?== Both [[-1,-2,-3],[-4,-5]])]

ex14_fail = conjoin [$(testing' [|fmap not (Both [Nothing])|]) (?== Both [Nothing])
                    ,$(testing' [|fmap not (Both []) :: Both [] Maybe Bool|]) (?== Both [])]

ex15_pure = conjoin [$(testing' [|pure 1 :: Both [] Maybe Int|]) (?== Both [Just 1])
                    ,$(testing' [|pure "a" :: Both Maybe Validation String|]) (?==Both (Just (ok "a")))]

ex15_liftA2 = conjoin [$(testing' [|liftA2 (+) (Both (Just [10,100])) (Both (Just [1,2]))|]) (?==Both (Just [11,12,101,102]))
                      ,$(testing' [|liftA2 (+) (Both (Just [10,100])) (Both Nothing)|]) (?==Both Nothing)]

ex15_liftA2_2 = conjoin [$(testing' [|liftA2 (&&) (Both (Just (invalid "err"))) (Both (Just (pure True)))|]) (?==Both (Just (invalid "err")))
                        ,$(testing' [|liftA2 (&&) (Both (Just (invalid "err"))) (Both (Just (invalid "umm")))|]) (?==Both (Just (invalids ["err","umm"])))
                        ,$(testing' [|liftA2 (+) (Both [pure 1, invalid "fail 1"]) (Both [pure 10, pure 100, invalid "fail 2"])|])
                         (?== Both [ok 11,ok 101,invalid "fail 2",
                                    invalid "fail 1",invalid "fail 1",
                                    invalids ["fail 1","fail 2"]])]
