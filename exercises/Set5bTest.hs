{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Set5bTest where

import Data.List
import Data.Maybe
import Test.QuickCheck

import Mooc.Th
import Mooc.Test

import Set5b

main = score tests

tests = [(1,"valAtRoot",[ex1_valAtRoot_Nothing, ex1_valAtRoot_Just])
        ,(2,"treeSize",[ex2_treeSize])
        ,(3,"treeMax",[ex3_small, ex3_large])
        ,(4,"allValues",[ex4_small, ex4_large])
        ,(5,"mapTree",[ex5_small, ex5_large])
        ,(6,"cull",[ex6_small, ex6_large])
        ,(7,"isOrdered",[ex7_small, ex7_large])
        ,(8,"walk",[ex8_small, ex8_large])
        ,(9,"set",[ex9_small, ex9_medium, ex9_large])
        ,(10,"search",[ex10_small, ex10_large])
        ]

-- -- -- -- -- -- -- --


treeOfSize :: Arbitrary a => Int -> Gen (Tree a)
treeOfSize 0 = return Empty
treeOfSize siz = do
  let siz' = siz-1
  sizl <- choose (0,siz')
  let sizr = siz'-sizl
  l <- treeOfSize sizl
  r <- treeOfSize sizr
  v <- arbitrary
  return $ Node v l r

ex1_valAtRoot_Nothing = $(testing [|valAtRoot (Empty :: Tree Bool)|]) (?== (Nothing :: Maybe Bool))

ex1_valAtRoot_Just = property $ do
  l <- treeOfSize 2 :: Gen (Tree Integer)
  r <- treeOfSize 2 :: Gen (Tree Integer)
  v <- choose (0,10 :: Integer)
  let t = Node v l r
  return $ $(testing [|valAtRoot t|]) (?== Just v)

ex2_treeSize =
  forAllShrink_ (choose (0,50)) $ \s ->
  forAllBlind (treeOfSize s :: Gen (Tree Integer)) $ \t ->
    $(testing [|treeSize t|]) (?==s)

treeAndMax :: Int -> Gen (Int, Tree Int)
treeAndMax 0 = return (0,Empty)
treeAndMax d = do
  (lmax,l) <- treeAndMax (d-1)
  (rmax,r) <- treeAndMax (d-1)
  v <- choose (0,20)
  return (maximum [lmax,rmax,v], (Node v l r))

m_ex3 depth = forAllBlind (treeAndMax depth) $ \(m,tree) ->
  $(testing [|treeMax tree|]) (?==m)

ex3_small = conjoin [m_ex3 0
                    ,m_ex3 1
                    ,m_ex3 2]
ex3_large = conjoin [m_ex3 3
                    ,m_ex3 4]

tree3 a b c = Node b (Node a Empty Empty) (Node c Empty Empty)
tree7 [a,b,c,d,e,f,g] = Node d (tree3 a b c) (tree3 e f g)


ex4_small = property $ do
  ~[a,b,c] <- vectorOf 3 (choose (0,4::Int))
  let input = tree3 a b c
      out = all (>0) [a,b,c]
  return $ counterexample ("allValues (>0) "++show' input) $ allValues (>0) input ?== out

ex4_large = property $ do
  vals <- vectorOf 7 (choose (0,8::Int))
  let input = tree7 vals
      out = all (>1) vals
  return $ counterexample ("allValues (>0) "++show' input) $ allValues (>1) input ?== out

ex5_small = property $ do
  ~[a,b,c] <- vectorOf 3 (choose (0,4::Int))
  let input = tree3 a b c
      output = tree3 (a+1) (b+1) (c+1)
  return $ counterexample ("mapTree (+1) "++show' input) $ mapTree (+1) input ?== output

linearLeft [] = Empty
linearLeft (x:xs) = Node x (linearLeft xs) Empty

linearRight [] = Empty
linearRight (x:xs) = Node x Empty (linearRight xs)

ex5_large = property $ do
  cs <- listOf1 (choose ('a','c'))
  ds <- listOf1 (choose ('a','c'))
  e <- choose ('a','c')
  let input = Node e (linearRight cs) (linearLeft ds)
      f = (=='a')
      output = Node (f e) (linearRight $ map f cs) (linearLeft $ map f ds)
  return $ counterexample ("mapTree (=='a') "++show' input) $ mapTree (=='a') input ?== output

treeWithout :: (Eq a, Arbitrary a) => Int -> a -> Gen (Tree a)
treeWithout 0 _ = return Empty
treeWithout depth v = oneof [return Empty
                            ,do l <- treeWithout (depth-1) v
                                r <- treeWithout (depth-1) v
                                v' <- arbitrary `suchThat` (/=v)
                                return $ Node v' l r]

ex6_small = conjoin [$(testing [|cull 2 (Node 1 (Node 2 Empty Empty) (Node 0 Empty Empty))|])
                     (?==Node 1 Empty (Node 0 Empty Empty))
                    ,$(testing [|cull 3 (Node 1 (Node 2 Empty Empty) (Node 0 Empty Empty))|])
                     (?==Node 1 (Node 2 Empty Empty) (Node 0 Empty Empty))
                    ,$(testing [|cull 0 (Node 1 (Node 0 Empty (Node 2 Empty Empty)) (Node 3 Empty (Node 0 Empty Empty)))|])
                     (?==Node 1 Empty (Node 3 Empty Empty))]

replaceLeaf :: Tree a -> Tree a -> Gen (Tree a)
replaceLeaf t Empty = return t
replaceLeaf t (Node val l r) = oneof [fmap (\l' -> Node val l' r) (replaceLeaf t l)
                                     ,fmap (\r' -> Node val l r') (replaceLeaf t r)]


ex6_large = property $ do
  v <- arbitrary :: Gen Int
  output <- treeWithout 3 v
  ~(Node _ l r) <- treeOfSize 3
  let addition = Node v l r
  input <- replaceLeaf addition output
  input2 <- replaceLeaf addition input
  return $ conjoin [$(testing [|cull v input|]) (?==output)
                   ,$(testing [|cull v input2|]) (?==output)]

orderedTree :: Int -> (Int,Int) -> Gen (Tree Int)
orderedTree 0 _ = return Empty
orderedTree depth (min,max)
  | min<max = frequency [(1, return Empty)
                        ,(4, do v <- choose (min,max-1)
                                l <- orderedTree (depth-1) (min,v)
                                r <- orderedTree (depth-1) (v+1,max)
                                return $ Node v l r)]
orderedTree _ _ = return Empty

try :: [Gen (Maybe a)] -> Gen (Maybe a)
try [] = return $ Nothing
try (g:gs) = do m <- g
                if isJust m then return m else try gs

breakOrder :: Tree Int -> Gen (Maybe (Tree Int))
breakOrder Empty = return Nothing
breakOrder (Node v Empty Empty) = return Nothing
breakOrder (Node v l r) = try [do ml <- breakOrder l
                                  case ml of Nothing -> return Nothing
                                             Just l' -> return $ Just $ Node v l' r
                              ,do mr <- breakOrder r
                                  case mr of Nothing -> return Nothing
                                             Just r' -> return $ Just $ Node v l r'
                              ,return (Just (Node v r l))]

m_ex7 depth = forAllBlind (orderedTree depth (0,100)) $ \t ->
  forAllBlind (breakOrder t) $ \b ->
  conjoin [$(testing [|isOrdered t|]) (?==True)
          ,isJust b ==> $(testing [|isOrdered (fromJust b)|]) (?==False)]

ex7_small = conjoin [$(testing [|isOrdered (Empty::Tree Int)|]) (?==True)
                    ,forAllBlind (choose (0,10::Int)) $ \i -> $(testing [|isOrdered (Node i Empty Empty)|]) (?==True)
                    ,m_ex7 2]

ex7_large = conjoin [m_ex7 3
                    ,m_ex7 4
                    ,m_ex7 5]


pathAndTree :: (Eq a, Arbitrary a) => Int -> a -> Gen ([Step],Tree a)
pathAndTree 0 v = return ([],Node v Empty Empty)
pathAndTree depth v = oneof [do l <- treeWithout (depth-1) v
                                v' <- arbitrary `suchThat` (/=v)
                                (p,r) <- pathAndTree (depth-1) v
                                return (StepR:p,Node v' l r)
                            ,do r <- treeWithout (depth-1) v
                                v' <- arbitrary `suchThat` (/=v)
                                (p,l) <- pathAndTree (depth-1) v
                                return (StepL:p,Node v' l r)
                            ,do l <- treeWithout (depth-1) v
                                r <- treeWithout (depth-1) v
                                return ([],Node v l r)]

m_ex8 d = forAll_ $ \(v::Int) ->
  forAllBlind (pathAndTree d v) $ \(p,t) ->
  $(testing [|walk p t|]) (?==Just v)

ex8_small = conjoin [$(testing [|walk [StepL] (Empty :: Tree Char)|]) (?==Nothing)
                    ,forAll_ $ \(a::Int,b::Int,c::Int) ->
                        $(testing [|walk [StepL,StepR] (Node a (Node b Empty Empty) (Node c Empty Empty))|]) (?==Nothing)
                    ,m_ex8 0
                    ,m_ex8 1
                    ,m_ex8 2]

ex8_large = conjoin [m_ex8 3
                    ,m_ex8 4
                    ,m_ex8 5]


ex9_small = conjoin [$(testing [|set ([]::[Step]) 'a' (Empty::Tree Char)|]) (?==Empty)
                    ,$(testing [|set ([]::[Step]) True (Node False Empty Empty)|]) (?==Node True Empty Empty)
                    ,$(testing [|set [StepL] True (Node False Empty Empty)|]) (?==Node False Empty Empty)
                    ,$(testing [|set [StepL] True (Node False (Node False Empty Empty) Empty)|]) (?==Node False (Node True Empty Empty) Empty)]

setList xs i v = before ++ v:after
  where (before,_:after) = splitAt i xs

ex9_medium = property $ do
  vals <- vectorOf 7 (choose (0,10::Int))
  (i,path) <- elements [(0,[StepL,StepL])
                       ,(1,[StepL])
                       ,(2,[StepL,StepR])
                       ,(3,[])
                       ,(4,[StepR,StepL])
                       ,(5,[StepR])
                       ,(6,[StepR,StepR])]
  new <- choose (11,20::Int)
  let inp = tree7 vals
      vals' = setList vals i new
      out = tree7 vals'
  return $ $(testing [|set path new inp|]) (?==out)

ex9_large = forAllShrink (choose (1,10)) shrinkPositive $ \len -> property $ do
  (dir,mk) <- elements [(StepL,linearLeft)
                       ,(StepR,linearRight)]
  cs <- vectorOf len (choose ('a','z'))
  i <- choose (0,len - 2)
  new <- choose ('a','z')
  return $ $(testing [|set (replicate i dir) new (mk cs)|]) (?==mk (setList cs i new))

m_ex10_yes depth = forAll_ $ \(val::Int) ->
  forAllBlind (pathAndTree depth val) $ \(path,tree) ->
  $(testing [|search val tree|]) (?==Just path)

m_ex10_no depth = forAll_ $ \(val::Int) ->
  forAllBlind (treeWithout depth val) $ \tree ->
  $(testing [|search val tree|]) (?==Nothing)

ex10_small = conjoin [m_ex10_yes 0
                     ,m_ex10_no 0
                     ,m_ex10_yes 1
                     ,m_ex10_no 1
                     ,m_ex10_yes 2
                     ,m_ex10_no 2]

ex10_large = conjoin [m_ex10_yes 3
                     ,m_ex10_no 3
                     ,m_ex10_yes 4
                     ,m_ex10_no 4
                     ,m_ex10_yes 5
                     ,m_ex10_no 5]
