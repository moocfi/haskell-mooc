{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Set10aTest where

import Mooc.Test
import Mooc.Th

import Control.Exception (evaluate, try, SomeException)
import Data.Char
import Data.List
import Language.Haskell.TH
import Test.QuickCheck
import Test.QuickCheck.Monadic


import Set10a hiding (describe,move,play)

main = score tests

tests = [(1,"doublify",[ex1_finite, ex1_infinite])
        ,(2,"interleave",[ex2_finite, ex2_infinite_1, ex2_infinite_2])
        ,(3,"deal",[ex3_examples, ex3_finite, ex3_infinite_1, ex3_infinite_2])
        ,(4,"averages",[ex4_simple, ex4_finite, ex4_infinite])
        ,(5,"alternate",[ex5_examples, ex5])
        ,(6,"lenghtAtLeast",[ex6_finite, ex6_infinite])
        ,(7,"chunks",[ex7_finite, ex7_infinite])
        ,(8,"IgnoreCase",[ex8_type, ex8_works])
        ,(9,"maze",[ex9_1, ex9_2])]

-- -- -- --

ex1_finite =
  forAllBlind (choose (0,10)) $ \n ->
  forAllBlind (choose ('a','z')) $ \c ->
  forAllBlind (choose ('a','z')) $ \d ->
  $(testing [|doublify (replicate n c ++ [d])|]) (?==(replicate (2*n) c ++ [d,d]))

ex1_infinite =
  forAllBlind (choose (0,100)) $ \n ->
  forAllBlind (choose (0,1000)) $ \i ->
  counterexample ("With n = "++show n) $
  $(testing' [|doublify [n..]|]) $ \was ->
  counterexample (" Index "++show i) $
  was !! i ?== n + div i 2

evens (x:xs) = odds xs
evens [] = []
odds (x:xs) = x:evens xs
odds [] = []

ex2_finite = forAll_ $ \(is::[Integer]) ->
  forAll_ $ \(rest::[Integer]) ->
  let as = odds is
      bs = evens is
  in conjoin [$(testing [|interleave as bs|]) (?==is)
             ,if length as > length bs
              then $(testing [|interleave (as++rest) bs|]) (?==is++rest)
              else $(testing [|interleave as (bs++rest)|]) (?==is++rest)]

ex2_infinite_1 = forAll_ $ \(i::Integer,j::Integer) ->
  counterexample ("with i = " ++ show i ++ ", j = " ++ show j) $
  $(testing' [|take 10 (interleave (repeat i) (repeat j))|]) (?==take 10 (cycle [i,j]))

ex2_infinite_2 = forAll_ $ \(x::Integer) ->
  forAll_ $ \(Positive i) ->
  counterexample ("with x = " ++ show x) $
  $(testing' [|interleave [0..] (repeat x)|]) $ \res ->
  counterexample ("  element at index " ++ show i) $
  res !! i == if odd i then x else fromIntegral (div i 2)

ex3_examples =
  conjoin [$(testing [|deal ["Hercule","Ariadne"] ["Ace","Joker","Heart"]|]) (?==[("Ace","Hercule"),("Joker","Ariadne"),("Heart","Hercule")])
          ,$(testing [|deal ["Hercule","Ariadne"] e|]) (?==[])]
  where e = [] :: [String]

word = listOf1 (choose ('a','z'))

ex3_finite = forAllShrink_ (listOf1 word) $ \names ->
  forAllShrink_ (listOf1 word) $ \cards ->
  and [not (null names), not (null cards)] ==>
  $(testing [|deal names cards|]) (?==[(cards!!i, names!!(mod i (length names))) | i <- [0..length cards - 1]])

ex3_infinite_1 = forAllShrink_ (listOf1 word) $ \names ->
  not (null names) ==>
  forAllBlind (choose (length names, 10*length names)) $ \n ->
  counterexample ("With n = "++show n++", names = "++show names) $
  $(testing' [|take n (deal names (map show [0..]))|]) (?==[(show i, names!!(mod i (length names))) | i <- [0..n - 1]])

ex3_infinite_2 =
  $(testing' [|take 10 (deal (repeat "me") (repeat "card"))|]) (?==replicate 10 ("card","me"))



ex4_simple = conjoin [$(testing [|averages [1.0,2.0,3.0]|]) (approximateListEq [1.0,(1+2)/2,(1+2+3)/3])
                     ,$(testing [|averages [7,2,5,8]|]) (approximateListEq [7,(7+2)/2,(7+2+5)/3,(7+2+5+8)/4])
                     ,let e = [] :: [Double] in $(testing [|averages e|]) (?==e)]

ex4_finite = forAllBlind (elements [0,1,2,3]) $ \base ->
  forAllBlind (elements [1,2,3,4,5,6]) $ \step ->
  forAllBlind (choose (2,7)) $ \len ->
  let input = [base + i*step | i <- [0..len]]
      output = [(j*base + (j-1)*j*step/2)/j | j <- [1..len+1]]
  in $(testing [|averages input|]) (approximateListEq output)

ex4_infinite = forAllBlind (elements [0,1,2,3,4,5,6,7,8,9]) $ \a ->
  forAllBlind (elements [0,1,2,3,4,5,6,7,8,9]) $ \b ->
  forAllBlind (choose (0,1000)) $ \i ->
  counterexample ("With a=" ++ show a ++ ", b=" ++ show b) $
  $(testing' [|averages (cycle [a,b])|]) $ \res ->
  counterexample ("  element at index " ++ show i) $
  let na = fromIntegral (div i 2 + 1)
      nb = fromIntegral (div (i+1) 2)
  in res !! i ?~= (na*a + nb*b)/(na+nb)

ex5_examples = conjoin [$(testing' [|take 20 (alternate "abc" "def" ',')|]) (?=="abc,def,abc,def,abc,")
                       ,$(testing' [|take 10 (alternate [1,2] [3,4,5] 0)|]) (?==[1,2,0,3,4,5,0,1,2,0])]

ex5 = forAllBlind (choose (1,3)) $ \n ->
  forAllBlind (choose (1,6)) $ \m ->
  forAll_ $ \(NonNegative i) ->
  $(testing [|alternate (replicate n 1) (replicate m 1) 0|]) $ \was ->
    counterexample (" Index "++show i) $
    (was !! i ?== if mod i (2+n+m) `elem` [n,n+1+m] then 0 else 1)

ex6_finite = forAll_ $ \(is::[Int]) ->
  forAll_ $ \(NonNegative n) ->
  $(testing [|lengthAtLeast n is|]) (?==(length is >= n))

ex6_infinite = forAll_ $ \(Positive (i::Int)) ->
  forAll_ $ \(NonNegative n) ->
  counterexample ("With n = " ++ show n ++ ", i = " ++ show i) $
  $(testing' [|lengthAtLeast n (repeat i)|]) (?==True)

ex7_finite = forAllBlind (choose (1,6)) $ \n ->
  forAllShrink_ (listOf word) $ \ws ->
  $(testing [|chunks n ws|]) (?==[[ws!!i | i <- [j..j+n-1]] | j <- [0..length ws - n]])

ex7_infinite = forAllBlind (choose (1,10)) $ \n ->
  forAllBlind (choose (1,25)) $ \k ->
  counterexample ("With k = "++show k++", n = "++show n) $
  $(testing' [|take k (chunks n [0..])|]) (?==[[j..j+n-1] | j <- [0..k-1]])

shuffleCase w = (do s <- vectorOf (length w) (elements [toLower,toUpper])
                    return (zipWith ($) s w)) `suchThat` (/=w)

ex8_type = $(do let s = "IgnoreCase"
                n <- lookupTypeName s
                case n of
                  Nothing -> [|counterexample ("Type "++s++" not defined!") False|]
                  Just n -> do info <- reify n
                               case info of TyConI (NewtypeD _ _ _ _ _ _) -> [|property True|]
                                            _ -> [|counterexample ("Definition "++s++" is not a newype declaration!") False|])

ex8_works =
  $(hasType' "ignorecase" "String -> IgnoreCase") $ \ignorecase ->
  $(withInstance "Eq" "IgnoreCase" [|(==)|]) $ \((==)) ->
  property $ do
  w1 <- word
  w2 <- shuffleCase w1
  w3 <- word
  let t :: String -> String -> Bool -> Property
      t x y exp = counterexample ("ignorecase " ++ show x ++ " == ignorecase " ++ show y) $ (ignorecase x == ignorecase y) ?== exp
  return $ conjoin [t w1 w1 True
                   ,t w1 w2 True
                   ,t w2 w1 True
                   ,t (w1++w3) w2 False
                   ,t w2 (w1++w3) False
                   ,t w1 (w2++w3) False
                   ,t (w2++w3) w1 False]


describe :: Room -> String
describe (Room s _) = s

move :: Room -> String -> Maybe Room
move (Room _ directions) direction = lookup direction directions

play :: Room -> [String] -> [String]
play room [] = [describe room]
play room (d:ds) = case move room d of Nothing -> [describe room]
                                       Just r -> describe room : play r ds

ex9_1 = forAllShrink_ (choose (0,20)) $ \i ->
  counterexample ("with i = " ++ show i) $
  conjoin [$(testing' [|play maze (replicate i "Left")|]) (?==take (i+1) (cycle ["Maze","Deeper in the maze","Elsewhere in the maze"]))
          ,$(testing' [|play maze (replicate i "Right")|]) (?==take (i+1) (cycle ["Maze","Elsewhere in the maze","Deeper in the maze"]))]

ex9_2 = forAllShrinkBlind (listOf (elements ["Left","Right"])) subterms $ \dirs ->
  let cnt = sum (map (\d -> case d of "Left" -> 1; "Right" -> -1) dirs)
      answer = ["Maze","Deeper in the maze","Elsewhere in the maze"] !! mod cnt 3
  in counterexample ("with dirs = " ++ show dirs) $
  $(testing' [|last (play maze dirs)|]) (?==answer)
