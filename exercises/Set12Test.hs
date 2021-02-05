{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, StandaloneDeriving #-}

module Set12Test where

import Mooc.Test
import Mooc.Th
import Data.List
import Data.Monoid

import Test.QuickCheck hiding (Result,Fun,Failure)

import Set12

main = score tests

tests = [(1,"incrementAll",[ex1_list, ex1_just, ex1_nothing])
        ,(2,"fmap23",[ex2_fmap2_examples, ex2_fmap3_examples])
        ,(3,"Functor Result",[ex3_num, ex3_empties])
        ,(4,"Functor List",[ex4_num, ex4_bool])
        ,(5,"Functor TwoList",[ex5_num, ex5_bool])
        ,(6,"count",[ex6_list, ex6_maybe])
        ,(7,"inBoth",[ex7_list, ex7_maybe])
        ,(8,"Foldable List",[ex8_bool, ex8_num])
        ,(9,"Foldable TwoList",[ex9_bool, ex9_num])
        ,(10,"Fun",[ex10_1, ex10_2])
        ,(11,"Foldable Tree",[ex11_fmap_even, ex11_fmap_plus, ex11_sumTree_int, ex11_sumTree_list])]


-- -- -- -- -- --

ex1_list =
  forAll_ $ \(xs::[Int]) ->
  $(testing [|incrementAll xs|]) (?== map (+1) xs)

ex1_just =
  forAll_ $ \(i::Int) ->
  $(testing [|incrementAll (Just i)|]) (?== Just (i+1))

ex1_nothing = $(testing [|incrementAll (Nothing::Maybe Int)|]) (?== Nothing)

ex2_fmap2_examples =
  conjoin [$(testing' [|fmap2 negate [[1,2],[3]]|]) (?==[[-1,-2],[-3]])
          ,$(testing' [|fmap2 head [Just "abcd",Nothing,Just "efgh"]|]) (?==[Just 'a',Nothing,Just 'e'])]

ex2_fmap3_examples =
  conjoin [$(testing' [|fmap3 negate [[[1,2],[3]],[[4],[5,6]]]|]) (?==[[[-1,-2],[-3]],[[-4],[-5,-6]]])
          ,$(testing' [|fmap3 not (Just [Just False, Nothing])|]) (?==Just [Just True,Nothing])]

deriving instance Eq a => Eq (Result a)

ex3_num =
  forAll_ $ \(k::Int) ->
  counterexample ("fmap (+1) (MkResult "++show k++")") $
  fmap (+(1::Int)) (MkResult k) ?== MkResult (k+1)

ex3_empties =
  $(testing' [|fmap not NoResult|]) (?== NoResult)
  .&.
  $(testing' [|fmap not (Failure "oh no.")|]) (?== Failure "oh no.")
  .&.
  $(testing' [|fmap not (Failure "Mein Gott!")|]) (?== Failure "Mein Gott!")

deriving instance Eq a => Eq (List a)

toList = foldr LNode Empty

ex4_num =
  forAll_ $ \(xs::[Int]) ->
  let l = toList xs
  in counterexample ("fmap (+1) ("++show l++")") $
    fmap (+1) l ?== toList (map (+1) xs)

ex4_bool =
  forAll_ $ \(bs::[Bool]) ->
  let l = toList bs in
  counterexample ("fmap not ("++show l++")") $
    fmap not l ?== toList (map not bs)

deriving instance Eq a => Eq (TwoList a)

toTwoList :: [a] -> TwoList a
toTwoList [] = TwoEmpty
toTwoList (x:y:xs) = TwoNode x y (toTwoList xs)

ex5_num =
  forAll_ $ \(xs::[Int]) ->
  even (length xs) ==>
  let l = toTwoList xs
  in counterexample ("fmap (+1) ("++show l++")") $
    fmap (+1) l ?== toTwoList (map (+1) xs)

ex5_bool =
  forAll_ $ \(bs::[Bool]) ->
  even (length bs) ==>
  let l = toTwoList bs in
  counterexample ("fmap not ("++show l++")") $
    fmap not l ?== toTwoList (map not bs)

ex6_list =
  forAll_ $ \(bs::[Bool]) ->
  forAll_ $ \b ->
  $(testing [|count b bs|]) (?==length (filter (==b) bs))

ex6_maybe =
  forAllBlind (choose ('a','z')) $ \c ->
  forAllBlind (choose ('a','z') `suchThat` (/=c)) $ \d ->
  conjoin [$(testing [|count c n|]) (?==0)
          ,$(testing [|count c (Just c)|]) (?==1)
          ,$(testing [|count d (Just c)|]) (?==0)]
  where n = Nothing :: Maybe Char

ex7_list =
  forAll_ $ \(is::[Int]) ->
  forAll_ $ \(js::[Int]) ->
  $(testing [|inBoth (nub is) (nub js)|]) (?==intersect (nub is) (nub js))

ex7_maybe =
  forAll_ $ \(i::Int) ->
  forAll_ $ \j ->
  conjoin [$(testing [|inBoth (Just i) (Just j)|]) (?==if i==j then [i] else [])
          ,$(testing [|inBoth (Just i) (Just i)|]) (?==[i])
          ,$(testing [|inBoth (Just i) n|]) (?==[])]
  where n = Nothing :: Maybe Int

ex8_bool =
  forAll_ $ \(xs::[Bool]) ->
  let l = toList xs
  in counterexample ("foldr (:) [] ("++show l++")") $
    foldr (:) [] l ?== xs

ex8_num =
  forAll_ $ \(NonEmpty (is::[Int])) ->
  let l = toList is
  in $(testing [|minimum l|]) (?==minimum is)

ex9_bool =
  forAll_ $ \(xs::[Bool]) ->
  even (length xs) ==>
  let l = toTwoList xs
  in counterexample ("foldr (:) [] ("++show l++")") $
    foldr (:) [] l ?== xs

ex9_num =
  forAll_ $ \(NonEmpty (is::[Int])) ->
  even (length is) ==>
  let l = toTwoList is
  in $(testing [|minimum l|]) (?==minimum is)

ex10_1 =
  forAll_ $ \(i::Int) ->
  counterexample ("runFun (fmap not (Fun even)) "++show i) $
    runFun (fmap not (Fun even)) i ?== odd i

ex10_2 =
  forAll_ $ \(i::Int) ->
  counterexample ("runFun (fmap (*2) (Fun (\\i -> i))) "++show i) $
    runFun (fmap (*2) (Fun id)) i ?== 2*i

deriving instance Eq a => Eq (Tree a)

ex11_fmap_even = conjoin
  [$(testing' [|fmap even Leaf|]) (?==Leaf)
  ,$(testing' [|fmap even (Node 3 Leaf Leaf)|]) (?==Node False Leaf Leaf)
  ,$(testing' [|fmap even (Node 2 Leaf Leaf)|]) (?==Node True Leaf Leaf)
  ,$(testing' [|fmap even (Node 2 (Node 3 Leaf Leaf) Leaf)|]) (?==Node True (Node False Leaf Leaf) Leaf)
  ,$(testing' [|fmap even (Node 2 (Node 3 Leaf Leaf) (Node 4 Leaf Leaf))|]) (?==Node True (Node False Leaf Leaf) (Node True Leaf Leaf))]

trees :: Int -> Gen (Tree Int, Tree Int)
trees 0 = return (Leaf,Leaf)
trees n = do
  i <- choose (0,10)
  let j = i+2
  nleft <- choose (0,n-1)
  nright <- choose (0,n-1)
  (iLeft,jLeft) <- trees nleft
  (iRight,jRight) <- trees nright
  return (Node i iLeft iRight, Node j jLeft jRight)

m_fmap n = forAllBlind (trees n) $ \(from,to) ->
  counterexample ("fmap (+2) ("++show from++")") $
  fmap (+2) from ?== to

ex11_fmap_plus = conjoin [m_fmap 0
                         ,m_fmap 1
                         ,m_fmap 2
                         ,m_fmap 3
                         ,m_fmap 4]

treeAndSum 0 = return (0,Leaf)
treeAndSum n = do
  (lsum,l) <- treeAndSum (n-1)
  (rsum,r) <- treeAndSum (n-1)
  val <- choose (0,10::Int)
  return (val+rsum+lsum, Node (Sum val) l r)

m_treeSum_Sum n = forAllBlind (treeAndSum n) $ \(s,t) ->
  $(testing [|sumTree t|]) (?==Sum s)

ex11_sumTree_int = conjoin [m_treeSum_Sum 0
                           ,m_treeSum_Sum 1
                           ,m_treeSum_Sum 2
                           ,m_treeSum_Sum 3]

split :: [a] -> Gen ([a],[a])
split xs = do
  i <- choose (0,length xs-1)
  return $ (take i xs, drop i xs)

listToTree 0 xs = return (Node xs Leaf Leaf)
listToTree n xs = frequency
  [(1,return (Node xs Leaf Leaf))
  ,(4,do
       (left,rest) <- split xs
       (me,right) <- split rest
       l <- listToTree (n-1) left
       r <- listToTree (n-1) right
       return $ Node me l r)]

m_sumTree_list n = forAll_ $ \(is::[Int]) ->
  forAllBlind (listToTree n is) $ \tree ->
  $(testing [|sumTree tree|]) (?==is)

ex11_sumTree_list = conjoin [m_sumTree_list 0
                            ,m_sumTree_list 1
                            ,m_sumTree_list 2
                            ,m_sumTree_list 3]
