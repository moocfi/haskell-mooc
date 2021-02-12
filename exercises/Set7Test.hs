{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, StandaloneDeriving, DeriveGeneric #-}

module Set7Test where

import Data.List
import Data.List.NonEmpty (NonEmpty ((:|)))
import GHC.Generics (Generic)
import Generic.Random
import Test.QuickCheck

import Mooc.Th
import Mooc.Test

import Set7 hiding (bake)

main = score tests

tests = [(1,"travel",[ex1_travel, ex1_velocity])
        ,(2,"Set",[ex2_member, ex2_add, ex2_empty_add])
        ,(3,"bake",[ex3_ok, ex3_finished, ex3_error, ex3_type])
        ,(4,"average NonEmpty",[ex4])
        ,(5,"reverseNonEmpty",[ex5_one, ex5_many])
        ,(6,"Semigroup x3",[ex6_Distance, ex6_Time, ex6_Velocity])
        ,(7,"Monoid Set",[ex7_combine, ex7_mempty])
        ,(8,"Operation1&2",[ex8_multiply1, ex8_multiply2, ex8_show1, ex8_show1_multiply1, ex8_show2, ex8_show2_multiply2])
        ,(9,"PasswordRequirement",[ex9_basics, ex9_either_both, ex9_large])
        ,(10,"Arithmetic",[ex10_examples, ex10_large])
        ]

--

ex1_velocity = forAll_ $ \d ->
  forAll_ $ \(Positive t) ->
  $(testing [|velocity (Distance d) (Time t)|]) (?==Velocity (d/t))

ex1_travel = forAll_ $ \(v,t) ->
  $(testing [|travel (Velocity v) (Time t)|]) (?==Distance (v*t))

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = fmap (Set . nub) orderedList

m2_member example = forAll_ $ \(Set xs) ->
  not (null xs) ==>
  forAllBlind (elements (xs`asTypeOf`example)) $ \x ->
  conjoin [$(testing [|member x (Set xs)|]) (?==True)
          ,$(testing [|member x (Set (delete x xs))|]) (?==False)]

ex2_member = conjoin [m2_member [True,False]
                     ,m2_member [1::Int,2,3]]

m2_add example = forAll_ $ \(Set xs) ->
  not (null xs) ==>
  forAllBlind (elements (xs`asTypeOf`example)) $ \x ->
  conjoin [$(testing [|add x (Set xs)|]) (?==(Set xs))
          ,$(testing [|add x (Set (delete x xs))|]) (?==(Set xs))]

ex2_add = conjoin [m2_add ["foo","bar"]
                  ,m2_add [1::Int,2,3]]

ex2_empty_add = conjoin
  [forAll_ $ \(i::Integer) -> counterexample ("add "++show i++" emptySet") (add i emptySet ?== Set [i])
  ,forAll_ $ \(b::Bool) -> counterexample ("add " ++show b++"(add "++show b++" emptySet)")
    (add b (add b emptySet) ?== Set [b])]

deriving instance Generic Event
instance Arbitrary Event where
  arbitrary = genericArbitrary uniform

ex3_good = [[AddEggs,AddFlour,AddSugar,Mix,Bake]
           ,[AddEggs,AddSugar,AddFlour,Mix,Bake]]

canFinish ev = any (\g -> ev `isPrefixOf` g) ex3_good
finished ev = any (\g -> g `isPrefixOf` ev) ex3_good

bake :: [Event] -> State
bake events = go Start events
  where go state [] = state
        go state (e:es) = go (step state e) es

ex3_ok = forAllBlind (elements ex3_good) $ \events ->
  $(testing [|bake events|]) (?==Finished)

ex3_finished = forAll_ $ \(ev::Event) ->
  $(testing [|step Finished ev|]) (?==Finished)

genBad = do
  g <- elements ex3_good
  i <- choose (0,length g - 1)
  let pre = take i g
  x <- arbitrary `suchThat` (\x -> not (canFinish (pre++[x]) || finished (pre++[x])))
  y <- arbitrary
  elements [pre++[x], pre++[x,y]]

ex3_error = forAllBlind (genBad) $ \events ->
  $(testing [|bake events|]) (?==Error)

ex3_type = $(reifyType "State") $ \(DataType vars cs) ->
  conjoin [counterexample "  should have no type parameters" $ vars == []
          ,conjoin $ map ok cs]
  where ok (Constructor _ []) = property True
        ok (Constructor n _) = counterexample ("  constructor "++show n++" should have no fields") $ property False
        ok (Weird n) = counterexample ("  constructor "++show n++" is weird. Make it normal.") $ property False

ex4 = conjoin [forAll_ $ \(Small i) -> $(testing [|average ((fromIntegral i :: Float) :| [])|]) (?==fromIntegral i)
              ,forAllBlind (choose (1,10)) $ \n -> forAll_ $ \(i::Rational) ->
                  $(testing [|average (i:|(replicate n i))|]) (?==i)
              ,forAll_ $ \(Small i) -> let f = fromIntegral i in $(testing [|average ((f-1):|[f,f+1])|]) (?==f)
              ,forAll_ $ \(Small i,Small j) ->
                  let x = fromIntegral i :: Double
                      y = fromIntegral j :: Double
                  in $(testing [|average (x:|(replicate 9 x ++ replicate 10 y))|]) (?~=(x+y)/2)]

ex5_one = forAll_ $ \(x::Bool) -> $(testing [|reverseNonEmpty (x:|[])|]) (?==(x:|[]))

ex5_many = forAllBlind (choose (0,10)) $ \(i::Int) ->
  forAllBlind (choose (2,10)) $ \(n::Int) ->
  $(testing [|reverseNonEmpty (i:|[i+1..i+n])|]) (?==((i+n):|[i+n-1,i+n-2..i]))

ex6_Distance = $(withInstance "Semigroup" "Distance" [|(<>)|]) $ \(<>) ->
  forAll_ $ \(x,y) ->
  $(testing [|(Distance x) <> (Distance y)|]) (?==Distance (x+y))

ex6_Time = $(withInstance "Semigroup" "Time" [|(<>)|]) $ \(<>) ->
  forAll_ $ \(x,y) ->
  $(testing [|(Time x) <> (Time y)|]) (?==Time (x+y))

ex6_Velocity = $(withInstance "Semigroup" "Velocity" [|(<>)|]) $ \(<>) ->
  forAll_ $ \(x,y) ->
  $(testing [|(Velocity x) <> (Velocity y)|]) (?==Velocity (x+y))

split [] = return ([],[])
split (x:xs) = do
  (as,bs) <- split xs
  oneof [return (x:as,bs)
        ,return (as,x:bs)
        ,return (x:as,x:bs)]

ex7_combine = $(withInstance1 "Monoid" "Set" [|(<>)|]) $ \(<>) ->
  forAll_ $ \(Set (xs :: [Integer])) ->
  forAllBlind (split xs) $ \(as,bs) ->
  $(testing [|(Set as) <> (Set bs)|]) (?==(Set xs))

ex7_mempty = $(withInstance1 "Monoid" "Set" [|((<>),mempty)|]) $ \((<>),mempty) ->
  forAll_ $ \(Set (xs :: [Char])) ->
  conjoin [counterexample (show (Set xs) ++ " <> mempty") ((Set xs <> mempty) ?== Set xs)
          ,counterexample ("mempty <> " ++ show (Set xs)) ((mempty <> Set xs) ?== Set xs)]

smallInt = choose (0,100::Int)

ex8_multiply1 = $(hasType "Multiply1" [t|Int -> Int -> Operation1|]) $ \mul1 ->
  forAllBlind smallInt $ \i ->
  forAllBlind smallInt $ \j ->
  $(testing [|compute1 (mul1 i j)|]) (?==(i*j))

ex8_multiply2 =
  $(withInstance "Operation2" "Multiply2" [|compute2|]) $ \compute2 ->
  $(withDefined "Multiply2") $ \mul2 ->
  forAllBlind smallInt $ \i ->
  forAllBlind smallInt $ \j ->
  -- no Show instance available...
  counterexample ("compute2 (Multiply2 " ++ show i ++ " " ++ show j ++ ")") $
  compute2 (mul2 i j) ?== (i*j)

ex8_show1 =
  $(hasType "show1" [t|Operation1 -> String|]) $ \show1 ->
  forAllBlind smallInt $ \i ->
  forAllBlind smallInt $ \j ->
  conjoin [$(testing [|show1 (Add1 i j)|]) (?==show i++"+"++show j)
          ,$(testing [|show1 (Subtract1 i j)|]) (?==show i++"-"++show j)]

ex8_show1_multiply1 =
  $(hasType "Multiply1" [t|Int -> Int -> Operation1|]) $ \mul1 ->
  $(hasType "show1" [t|Operation1 -> String|]) $ \show1 ->
  forAllBlind smallInt $ \i ->
  forAllBlind smallInt $ \j ->
  counterexample ("show1 (Multiply1 " ++ show i ++ " " ++ show j ++ ")") $
  show1 (mul1 i j) ?== show i++"*"++show j

ex8_show2 =
  conjoin [$(withDefined "show2") $ \show2 ->
              forAllBlind smallInt $ \i ->
              forAllBlind smallInt $ \j ->
              $(testing [|show2 (Add2 i j)|]) (?==show i++"+"++show j)
          ,$(withDefined "show2") $ \show2 ->
              forAllBlind smallInt $ \i ->
              forAllBlind smallInt $ \j ->
              $(testing [|show2 (Subtract2 i j)|]) (?==show i++"-"++show j)]

ex8_show2_multiply2 =
  $(withDefined "show2") $ \show2 ->
  $(withDefined "Multiply2") $ \mul2 ->
  forAllBlind (choose (0,100::Int)) $ \i ->
  forAllBlind (choose (0,100::Int)) $ \j ->
  $(testing [|show2 (Add2 i j)|]) (?==show i++"+"++show j)

ex9_basics =
  conjoin [$(testing [|passwordAllowed "short" (MinimumLength 8)|]) (?==False)
          ,$(testing [|passwordAllowed "veryLongPassword" (MinimumLength 8)|]) (?==True)
          ,$(testing [|passwordAllowed "password" (ContainsSome "0123456789")|]) (?==False)
          ,$(testing [|passwordAllowed "p4ssword" (ContainsSome "0123456789")|]) (?==True)
          ,$(testing [|passwordAllowed "password" (DoesNotContain "0123456789")|]) (?==True)
          ,$(testing [|passwordAllowed "p4ssword" (DoesNotContain "0123456789")|]) (?==False)
          ]

ex9_either_both = property $ do
  ~limits@[a,b,c,d] <- vectorOf 4 (choose (3,12))
  password <- vectorOf 24 (choose ('a','z'))
  let or = Or (Or (MinimumLength a) (MinimumLength b)) (Or (MinimumLength c) (MinimumLength d))
      and = And (And (MinimumLength a) (MinimumLength b)) (And (MinimumLength c) (MinimumLength d))
      low = minimum limits
      hi = maximum limits
  return $
    (low < hi) ==> do med <- choose (low,hi-1)
                      return $
                        conjoin [$(testing [|passwordAllowed (take (low-1) password) or|]) (?==False)
                                ,$(testing [|passwordAllowed (take (low-1) password) and|]) (?==False)
                                ,$(testing [|passwordAllowed (take med password) or|]) (?==True)
                                ,$(testing [|passwordAllowed (take med password) and|]) (?==False)
                                ,$(testing [|passwordAllowed (take hi password) or|]) (?==True)
                                ,$(testing [|passwordAllowed (take hi password) and|]) (?==True)]

tree leaf node [] = MinimumLength 0
tree leaf node cs
  | length cs <= 2 = leaf cs
tree leaf node cs = node (tree leaf node lefts) (tree leaf node rights)
  where (lefts,rights) = splitAt (length cs `div` 2) cs

special = elements "0123456789!#%&/()="

ex9_large = forAllBlind (listOf special) $ \forbidden ->
  forAllBlind (listOf special) $ \required' ->
  forAllShrink_ (listOf (choose ('a','z'))) $ \base ->
  let required = filter (not . flip elem forbidden) required'
      rules = And (tree ContainsSome Or required) (tree DoesNotContain And forbidden)
  in not (null required) ==>
    not (null forbidden) ==>
    forAllBlind (elements required) $ \req ->
    forAllBlind (elements forbidden) $ \forb ->
    conjoin [$(testing [|passwordAllowed base rules|]) (?==False)
             ,forAllBlind (shuffle (req:base)) $ \pw ->
                 $(testing [|passwordAllowed pw rules|]) (?==True)
             ,forAllBlind (shuffle (forb:req:base)) $ \pw ->
                 $(testing [|passwordAllowed pw rules|]) (?==False)]

ex10_examples = conjoin [$(testing [|evaluate (literal 3)|]) (?==3)
                        ,$(testing [|render   (literal 3)|]) (?=="3")
                        ,$(testing [|evaluate (operation "+" (literal 3) (literal 4))|]) (?==7)
                        ,$(testing [|render   (operation "+" (literal 3) (literal 4))|]) (?=="(3+4)")
                        ,$(testing [|evaluate (operation "*" (literal 3) (operation "+" (literal 1) (literal 1)))|]) (?==6)
                        ,$(testing [|render   (operation "*" (literal 3) (operation "+" (literal 1) (literal 1)))|]) (?=="(3*(1+1))")]

gen_arithmetic :: Integer -> Gen (Integer,Arithmetic,String,String)
gen_arithmetic 0 = do
  i <- choose (0,10)
  return (i,literal i,show i,"(literal "++show i++")")
gen_arithmetic i = do
  (lval,lexp,lshow,lcode) <- gen_arithmetic (i-1)
  (rval,rexp,rshow,rcode) <- gen_arithmetic (i-1)
  elements [(lval+rval,operation "+" lexp rexp,"("++lshow++"+"++rshow++")","(operation \"+\" "++lcode++" "++rcode++")")
           ,(lval*rval,operation "*" lexp rexp,"("++lshow++"*"++rshow++")","(operation \"*\" "++lcode++" "++rcode++")")]

m_ex10 level = forAllShrink_ (choose (1,level)) $ \l ->
  forAllBlind (gen_arithmetic level) $ \(val,exp,show,code) ->
  conjoin [counterexample ("evaluate "++code) $ evaluate exp ?== val
          ,counterexample ("render "++code) $ render exp ?== show]

ex10_large = conjoin [m_ex10 level | level <- [1..5]]
