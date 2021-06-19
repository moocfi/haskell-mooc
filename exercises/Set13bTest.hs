{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, StandaloneDeriving #-}

module Set13bTest where

import Mooc.Test
import Mooc.Th

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.Bits
import Data.Char
import Data.Either
import Data.Function
import Data.IORef
import Data.List
import qualified Data.Map as Map
import Test.QuickCheck hiding (Result,Failure)
import Test.QuickCheck.Monadic

import Examples.Bank
import Set13b hiding (perhapsIncrement,sumBounded,sumNotTwice)

main = score tests

tests = [(1,"ifM",[ex1_maybe, ex1_state])
        ,(2,"mapM2",[ex2_maybe, ex2_state])
        ,(3,"path",[ex3_visit_examples, ex3_visit_tree, ex3_visit_linear, ex3_visit
                   ,ex3_path_examples, ex3_path_linear, ex3_path])
        ,(4,"findSum2", [ex4_simple, ex4])
        ,(5,"allSums", [ex5_bits])
        ,(6,"foldM", [ex6_sumBounded_ok, ex6_sumBounded_fail, ex6_sumNotTwice])
        ,(7,"Monad Result", [ex7])
        ,(8,"Monad SL", [ex8_fmap_1, ex8_fmap_2, ex8_1, ex8_2, ex8_stress])
        ,(9,"mkCounter",[ex9_mkCounter])]

-- -- -- -- -- -- --

ex1_maybe =
  forAll_ $ \(c::Char) ->
  forAll_ $ \(d::Char) ->
  forAll_ $ \(b::Bool) ->
  let nc = Nothing :: Maybe Char
      nb = Nothing :: Maybe Bool
  in conjoin [$(testing [|ifM (Just b) (Just c) (Just d)|]) (?==Just (if b then c else d))
             ,$(testing [|ifM (Just b) (Just c) nc|]) (?==if b then Just c else Nothing)
             ,$(testing [|ifM (Just b) nc (Just d)|]) (?==if b then Nothing else Just d)
             ,$(testing [|ifM nb (Just c) (Just d)|]) (?==Nothing)]

ex1_state =
  conjoin [$(testing' [|runState (ifM get (return 'a') (return 'b')) True|]) (?==('a',True))
          ,$(testing' [|runState (ifM get (return 'a') (return 'b')) False|]) (?==('b',False))]

ex2_maybe =
  forAll_ $ \(as::[Int]) ->
  forAll_ $ \(bs::[Int]) ->
  let z = zipWith (-) as bs
      res = if all (/=0) z then Just z else Nothing
  in counterexample ("mapM2 (\\x y -> if x == y then Nothing else Just (x-y)) "++show as++" "++show bs) $
    mapM2 (\x y -> if x == y then Nothing else Just (x-y)) as bs ?== res

perhapsIncrement :: Bool -> Int -> State Int ()
perhapsIncrement True x = modify (+x)
perhapsIncrement False _ = return ()

ex2_state =
  forAll_ $ \(bs::[Bool]) ->
  forAll_ $ \(is::[Int]) ->
  counterexample ("runState (mapM2 perhapsIncrement "++show bs++" "++show is++") 0") $
  runState (mapM2 perhapsIncrement bs is) 0 ?== (zipWith (\_ _ -> ()) is bs, sum $ zipWith (\b i -> if b then i else 0) bs is)

ex3_visit_examples = conjoin [$(testing' [|runState (visit maze1 "Pit") []|]) (?==((),["Pit"]))
                             ,$(testing' [|runState (visit maze1 "Corridor 2") []|]) (?==((),["Corridor 3","Corridor 2"]))
                             ,$(testing' [|runState (visit maze1 "Entry") []|]) (?==((),["Dead end","Corridor 1","Pit","Entry"]))
                             ,$(testing' [|runState (visit maze1 "Entry") ["Corridor 1"]|]) (?==((),["Pit","Entry","Corridor 1"]))]

genTree 0 name = return [(name,[])]
genTree height name = do
  n <- choose (1,3)
  children <- nub <$> vectorOf n ((:name) <$> choose ('a','z'))
  rest <- concat <$> forM children (genTree (height-1))
  return $ (name,children):rest

shrinkTree xs = do pair@(x,_) <- xs
                   return $ map (\(n,ns) -> (n,delete x ns)) $ delete pair xs

ex3_visit_tree = forAllBlind (choose (1::Int,4)) $ \depth ->
  forAllShrinkBlind (genTree depth "0") shrinkTree $ \maze ->
  let root = fst (head maze)
  in counterexample ("runState (visit "++show maze++" "++show root++") []") $
  within timeLimit $
  ($ runState (visit maze root) []) . was $ \(_,state) ->
  counterexample " Final state" $
  hasElements (map fst maze) state

ex3_visit_linear = forAllBlind (choose (2::Int,6)) $ \n ->
  forAllBlind (choose (0,n-1)) $ \start ->
  forAllBlind (choose (start,n)) $ \end ->
  let maze = [(show i,[show (i+1)]) | i <- [0..n-1]]++[(show n,[])]
      sstart = show start
      send = show end
  in counterexample ("runState (visit "++show maze++" "++show sstart++") ["++show send++"]") $
  within timeLimit $
  ($ runState (visit maze sstart) [send]) . was $ \(_,state) ->
  counterexample " Final state" $
  hasElements (map show [start..end]) state

genConn :: [String] -> [String] -> Gen [(String,String)]
genConn nodes [] = return []
genConn nodes todo = do
  u <- elements todo
  v <- elements $ nodes \\ todo
  rest <- genConn nodes (delete u todo)
  return $ (u,v):(v,u):rest

genGraph :: [String] -> Gen [(String,String)]
genGraph nodes = do
  base <- genConn nodes (tail nodes)
  ns <- vectorOf 3 (elements nodes)
  return $ (ns!!0,ns!!1):(ns!!1,ns!!2):base

adj :: [(String,String)] -> [(String,[String])]
adj = map (simpl.unzip) . groupBy ((==)`on`fst) . sort . nub
  where simpl (x:_,ys) = (x,ys)

ex3_visit = property $ do
  n1 <- choose (1,5)
  n2 <- choose (1,4)
  names <- shuffle $ map show [1..n1+n2]
  let (names1,names2) = splitAt n1 names
  g1 <- genGraph names1
  g2 <- genGraph names2
  let maze = adj (g1++g2)
  return $ counterexample ("runState (visit "++show maze++" "++head names1++") []") $
    within timeLimit $
    ($ runState (visit maze (head names1)) []) . was $ \(_,state) ->
    counterexample " Final state" $
    hasElements names1 state

ex3_path_examples =
  conjoin [$(testing' [|path maze1 "Entry" "Pit"|]) (?==True)
          ,$(testing' [|path maze1 "Entry" "Dead end"|]) (?==True)
          ,$(testing' [|path maze1 "Pit" "Entry"|]) (?==False)
          ,$(testing' [|path maze1 "Entry" "Corridor 2"|]) (?==False)]

ex3_path_linear = forAllBlind (choose (2::Int,6)) $ \n ->
  forAllBlind (choose (0,n)) $ \start ->
  forAllBlind (choose (0,n)) $ \end ->
  let maze = [(show i,[show (i+1)]) | i <- [0..n-1]]++[(show n,[])]
      sstart = show start
      send = show end
  in $(testing [|path maze sstart send|]) (?==(start<=end))

ex3_path = property $ do
  n1 <- choose (1,5)
  n2 <- choose (1,5)
  names <- shuffle $ map show [1..n1+n2]
  let (names1,names2) = splitAt n1 names
  g1 <- genGraph names1
  g2 <- genGraph names2
  let maze = adj (g1++g2)
  from <- elements names
  to <- elements names
  return $ $(testing [|path maze from to|]) (?==(elem from names1 == elem to names1))

ex4_simple =
  conjoin [$(testing [|findSum2 [6,1] [7]|]) (hasElements [(1,6,7),(6,1,7)])
          ,$(testing [|findSum2 [6,1] [8]|]) (?==[])
          ,$(testing [|findSum2 [2,1] [4]|]) (hasElements [(2,2,4)])
          ,$(testing [|findSum2 [1,2,3,4] [6,7]|]) (hasElements [(2,4,6),(3,3,6),(3,4,7),(4,2,6),(4,3,7)])]

ex4 = forAllShrink_ (choose (1, 10)) $ \n ->
  (n>0 ==>) $
  property $ do
  step <- choose (1,5)
  start <- choose (1,100)
  let vals = [start,start+step..start+step*(n-1)]
  i1 <- choose (0,length vals-1)
  --i2 <- choose (0,n)
  let v1 = (vals!!i1)*2
      lim1 = min i1 (length vals-1-i1)
      triples1 = [(vals!!(i1-k),vals!!(i1+k),v1) | k <- [-lim1..lim1]]
  return $ $(testing [|findSum2 vals [v1]|]) (hasElements triples1)

ex5_bits = forAllShrink_ (choose (1::Int,9)) $ \n ->
  let vals = 0:map (\x -> 2^x) [0..n]
      all = nub $ [0..2^(n+1)-1]
  in conjoin [forAllBlind (elements vals) $ \del ->
                 del > 0 ==>
                 $(testing [|allSums (delete del vals)|]) (hasElementsDuplicates (filter ((==0).(Data.Bits..&.del)) all))
             ,$(testing [|allSums vals|]) (hasElementsDuplicates all)]

m is = maximum (scanl1 (+) is)

sumBounded :: Int -> [Int] -> Maybe Int
sumBounded k xs = foldM (f1 k) 0 xs

ex6_sumBounded_ok =
  forAllShrink_ (listOf1 (choose (-10,10))) $ \is ->
  forAllBlind (choose (0,3)) $ \diff ->
  let k = m is + diff
  in $(testing [|sumBounded k is|]) (?==Just (sum is))

ex6_sumBounded_fail =
  forAllBlind (listOf1 (choose (-10,10))) $ \is ->
  forAllBlind (choose (1::Int,3)) $ \diff ->
  let k = m is - diff
  in $(testing [|sumBounded k is|]) (?==Nothing)

sumNotTwice :: [Int] -> Int
sumNotTwice xs = fst $ runState (foldM f2 0 xs) []

ex6_sumNotTwice =
  forAllShrink_ (listOf1 (choose (-5,5))) $ \is ->
  $(testing [|sumNotTwice is|]) (?==sum (nub is))

ex7 =
  let op :: Int -> Result Int
      op i = if i>3 then Failure "big" else return (i+1)
      s = "let op i = if (i>3) then Failure \"big\" else return (i+1) in "
  in conjoin [counterexample' (s++" MkResult 1 >>= op") $
              (MkResult 1 >>= op) ?== MkResult 2,
              counterexample' (s++" MkResult 4 >>= op") $
              (MkResult 4 >>= op) ?== Failure "big",
              counterexample' (s++" Fail \"foo\" >>= op") $
              (Failure "foo" >>= op) ?== Failure "foo",
              counterexample' (s++" NoResult >>= op") $
              (NoResult >>= op) ?== NoResult]

word = listOf1 (choose ('a','z'))

ex8_fmap_1 = property $
  do i <- choose (0,10)
     let op = fmap (+1) getSL
     return $ counterexample ("runSL (fmap (+1) getSL) " ++ show i) $
       runSL op i ?== (i+1,i,[])

ex8_fmap_2 = property $
  do m <- word
     s <- choose (0,10)
     let op = fmap (const True) (msgSL m)
     return $ counterexample ("runSL (fmap (const True) (msgSL "++show m++")) "++show s) $
       runSL op s ?== (True,s,[m])

ex8_1 = property $
  do i <- choose (0,10)
     let op = putSL i >> getSL >>= \i -> msgSL (show i)
         s = "putSL "++show i++" >> getSL >>= \\i -> msgSL (show i)"
     return $ counterexample ("runSL ("++s++") 1") $ runSL op 1 ?== ((),i,[show i])

ex8_2 = property $
  do msg <- word
     msg2 <- word
     i <- choose (0,10)
     j <- choose (0,10)
     let op = do msgSL msg
                 x <- getSL
                 msgSL (msg2++show x)
                 putSL (x+i)
                 return x
         s = "op = \ndo msgSL "++show msg++"\n   x <- getSL\n   msgSL ("++show msg2++"++show x)\n   putSL (x+"++show i++")\n   return x"
     return $ counterexample (s++"\n\nrunSL op "++show j) $ runSL op j ?== (j,j+i,[msg,msg2++show j])

ex8_stress =
  forAll_ $ \ops ->
  let m (Left i) = modifySL (+i)
      m (Right s) = msgSL s
      s (Left i) = "modifySL (+"++show i++")"
      s (Right m) = "msgSL "++show m
      op = mapM_ m ops
      desc = "runSL ("++intercalate " >> " (map s ops)++") 0"
      (incs,msgs) = partitionEithers ops
      state = sum incs
  in counterexample desc $ runSL op 0 ?== ((),state,msgs)

ex9_mkCounter = forAllBlind (choose (0,20)) $ \n ->
  counterexample "(inc,get) <- mkCounter" $
  counterexample (" Called inc "++show n++" times, then called get") $
  monadicIO $ do m <- run $ do (i,g) <- mkCounter
                               replicateM_ n i
                               g
                 stop_ $ m ?== n
