{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Set9aTest where

import Mooc.Test
import Mooc.Th

import Data.List
import Data.List.NonEmpty (NonEmpty)
import Test.QuickCheck

import Set9a

main = score tests

tests = [(1,"workload",[ex1_small, ex1_medium, ex1_big])
        ,(2,"echo",[ex2_examples, ex2_random])
        ,(3,"countValid",[ex3])
        ,(4,"repeated",[ex4_examples, ex4_unrepeated_int, ex4_repeated_char, ex4_repeated_int])
        ,(5,"sumSuccess",[ex5])
        ,(6,"Lock",[ex6_examples, ex6_gen])
        ,(7,"Eq Text",[ex7])
        ,(8,"compose",[ex8_one_one, ex8_many_one, ex8_bijection, ex8_surjective])
        ,(9,"permute",[ex9_id, ex9_simple, ex9_compose])
        ]

-- -- -- -- --

ex1_small = forAllBlind (choose (1,10)) $ \n ->
  forAllBlind (choose (0,div 9 n)) $ \h ->
  $(testing [|workload n h|]) (?=="Piece of cake!")
ex1_medium = forAllBlind (choose (1,20)) $ \n ->
  forAllBlind (choose (div 10 n+1, div 99 n)) $ \h ->
  counterexample (show (div 10 n+1, div 99 n)) $
  $(testing [|workload n h|]) (?=="Ok.")
ex1_big = forAllBlind (choose (1,100)) $ \n ->
  forAllBlind (choose (div 100 n+1,101)) $ \h ->
  $(testing [|workload n h|]) (?=="Holy moly!")

ex2_examples = conjoin' [$(testing [|echo "hello!"|]) (?=="hello!, ello!, llo!, lo!, o!, !, ")
                        ,$(testing [|echo "ECHO"|]) (?=="ECHO, CHO, HO, O, ")
                        ,$(testing [|echo "X"|]) (?=="X, ")
                        ,$(testing [|echo ""|]) (?=="")]

word = listOf1 (choose ('a','z'))

ex2_random = forAllBlind word $ \w ->
  $(testing [|echo w|]) . was $ \v ->
  conjoin [counterexample "  number of ',' characters" $ (length (filter (==',') v)) ?== length w
          ,counterexample "  length of string" $ length v ?== div (length w * (length w + 1)) 2 + 2 * length w
          ,let prefix = w ++ ", " ++ tail w in counterexample ("  should start with "++show prefix) $ prefix `isPrefixOf` v]


digit = choose ('0','9')

valid = do
  a <- digit
  b <- digit
  c <- digit
  d <- digit
  x <- digit
  rest <- listOf digit
  elements [a:b:x:c:x:d:rest
           ,a:b:c:x:d:x:rest]

invalid = do
  a <- digit
  b <- digit
  c <- digit
  d <- digit
  e <- digit `suchThat` (/=c)
  f <- digit `suchThat` (/=d)
  rest <- listOf digit
  return $ a:b:c:d:e:f:rest

ex3 = forAllShrinkBlind (listOf valid) subterms $ \vs ->
  forAllShrinkBlind (listOf invalid) subterms $ \is ->
  forAllBlind (shuffle (vs++is)) $ \ns ->
  $(testing [|countValid ns|]) (?==length vs)

ex4_examples = conjoin
  [$(testing [|repeated [1,2,3]|]) (?==Nothing)
  ,$(testing [|repeated [1,2,2,3,3]|]) (?==Just 2)
  ,$(testing [|repeated [1,2,1,2,3,3]|]) (?==Just 3)]

ex4_unrepeated_int = forAll_ $ \ls ->
  $(testing [|repeated (nub (ls :: [Int]))|]) (?==Nothing)

ex4_repeated_char = forAllBlind (listOf1 (choose ('a','z'))) $ \cs' ->
  let cs = nub cs'
  in forAllBlind (choose (0,length cs - 1)) $ \i ->
    let c = cs!!i
        input = take i cs ++ c : drop i cs
    in $(testing [|repeated input|]) (?==Just c)

ex4_repeated_int = forAll_ $ \(NonEmpty prefix') ->
  forAll_ $ \suffix ->
  let prefix = nub prefix'
      chosen = last prefix :: Int
  in $(testing [|repeated (prefix ++ chosen : suffix)|]) (?==Just chosen)

ex5 = property $ do
  is <- arbitrary
  ws <- listOf word
  input <- shuffle (map Left ws ++ map Right is)
  return $ $(testing [|sumSuccess input|]) (?==if null is then Left "no data" else Right (sum is))

ex6_examples = conjoin
  [$(testing' [|isOpen aLock|]) (?== False)
  ,$(testing' [|isOpen (lock aLock)|]) (?== False)
  ,$(testing' [|isOpen (open "1234" aLock)|]) (?== True)
  ,$(testing' [|isOpen (lock (open "1234" aLock))|]) (?== False)
  ,$(testing' [|isOpen (open "1235" aLock)|]) (?== False)
  ,$(testing' [|isOpen (lock (open "1235" aLock))|]) (?== False)
  ,$(testing' [|isOpen (open "1234" (changeCode "0000" aLock))|]) (?== True)
  ,$(testing' [|isOpen (open "0000" (changeCode "0000" aLock))|]) (?== False)
  ,$(testing' [|isOpen (open "0000" (lock (changeCode "0000" (open "1234" aLock))))|]) (?== True)
  ,$(testing' [|isOpen (open "1234" (lock (changeCode "0000" (open "1234" aLock))))|]) (?== False)]

ex6_gen = property $ do
  code <- vectorOf 5 digit
  wrong <- vectorOf 5 digit `suchThat` (/=code)
  return $
    counterexample ("with code = " ++ show code) $
    conjoin [$(testing' [|isOpen (changeCode code (open "1234" aLock))|]) (?== True)
            ,$(testing' [|isOpen (lock (changeCode code (open "1234" aLock)))|]) (?== False)
            ,$(testing' [|isOpen (lock (lock (changeCode code (open "1234" aLock))))|]) (?== False)
            ,$(testing' [|isOpen (open code (lock (changeCode code (open "1234" aLock))))|]) (?== True)
            ,$(testing' [|isOpen (open code (open code (lock (changeCode code (open "1234" aLock)))))|]) (?== True)]
    .&&.
    counterexample ("with wrong = " ++ show wrong)
    (conjoin [$(testing' [|isOpen (open wrong (lock (changeCode code (open "1234" aLock))))|]) (?== False)
             ,$(testing' [|isOpen (open code (lock (changeCode code (changeCode wrong (open "1234" aLock)))))|]) (?== True)
             ,$(testing' [|isOpen (open wrong (lock (changeCode code (changeCode wrong (open "1234" aLock)))))|]) (?== False)
             ,$(testing' [|isOpen (open wrong (open code (lock (changeCode code (open "1234" aLock)))))|]) (?== True)
             ,$(testing' [|isOpen (open code (open wrong (lock (changeCode code (open "1234" aLock)))))|]) (?== True)])

spacify [] = elements [" ","\n",""]
spacify (c:cs) = frequency [(8, fmap (c:) (spacify cs))
                           ,(2, fmap (' ':) (spacify (c:cs)))
                           ,(1, fmap ('\n':) (spacify (c:cs)))]

change [] = do
  c <- choose ('a','z')
  return [c]
change (c:cs) = oneof [fmap (:cs) $ choose ('a','z') `suchThat` (/=c)
                      ,fmap (c:) $ change cs]

ex7 = $(withInstance "Eq" "Text" [|(==) :: Text -> Text -> Bool|]) $ \(==) ->
  property $ do
  bare <- word
  v1 <- spacify bare
  v2 <- spacify bare
  wrong <- change v2
  return $ conjoin [$(testing [|Text bare == Text v1|]) (?==True)
                   ,$(testing [|Text v1 == Text bare|]) (?==True)
                   ,$(testing [|Text v1 == Text v1|]) (?==True)
                   ,$(testing [|Text v1 == Text v2|]) (?==True)
                   ,$(testing [|Text wrong == Text v2|]) (?==False)
                   ,$(testing [|Text bare == Text wrong|]) (?==False)]

ex8_one_one = forAll_ $ \(x::Int) ->
  forAll_ $ \(y::Char) ->
  forAll_ $ \(z::String) ->
  $(testing [|compose [(x,y)] [(y,z)]|]) (?==[(x,z)])

ex8_many_one = forAll_ $ \(x::Int) ->
  forAll_ $ \(y::String) ->
  forAll_ $ \(z::Int) ->
  forAll_ $ \pairs ->
  let pairs' = (x,y):filter (\(k,v) -> k/=x && v/=y) pairs
  in forAllBlind (shuffle pairs') $ \pairs'' ->
    $(testing [|compose pairs'' [(y,z)]|]) (?==[(x,z)])

ex8_bijection = forAllShrink_ (choose (1,10)) $ \n ->
  forAllBlind (shuffle [1..n::Int]) $ \ys ->
  forAllBlind (shuffle [1..n::Int]) $ \zs ->
  forAllBlind (shuffle (zip [1..n] ys)) $ \xys ->
  forAllBlind (shuffle (zip ys zs)) $ \yzs ->
  $(testing [|compose xys yzs|]) (hasElements (zip [1..n] zs))

ex8_surjective = forAllShrink_ (choose (2,5)) $ \n ->
  forAllShrink_ (choose (2,5)) $ \k ->
  forAllBlind (vector n) $ \(output::[Int]) ->
  let xs = take (n*k) ['a'..]
      xys = zip xs (cycle [1..n])
      yzs = zip [1..n] output
      expected = zip xs (cycle output)
  in forAllBlind (shuffle xys) $ \xys' ->
    forAllBlind (shuffle yzs) $ \yzs' ->
    $(testing [|compose xys' yzs'|]) (hasElements expected)

ex9_id = property $ do
  n <- choose (1,5)
  let id = [0..n-1]
  xs <- shuffle (take n ['a'..'z'])
  return $ $(testing [|permute id xs|]) (?== xs)

ex9_simple = forAllBlind (sublistOf "pqrstuvwxyz") $ \original ->
  forAllBlind (shuffle original) $ \permuted ->
  let permutation = [i | x <- permuted, let Just i = elemIndex x original]
  in $(testing [|permute permutation permuted|]) (?==original)

ex9_compose = property $ do
  n <- choose (2,7)
  p <- shuffle [0..n-1]
  q <- shuffle [0..n-1]
  xs <- shuffle (take n ['a'..'z'])
  return $ counterexample
           ("permute (multiply p q) xs == permute p (permute q xs) failed:\n\
            \  p was " ++ show p ++ "\n  q was " ++ show q ++ ", and\n  xs \
            \was " ++ show xs ++ ";\n  permute (multiply p q) xs evaluated to " ++
            show (permute (multiply p q) xs) ++ ",\n  while \
            \permute p (permute q xs) evaluated to " ++
            show (permute p (permute q xs)))
           (permute (multiply p q) xs == permute p (permute q xs))
