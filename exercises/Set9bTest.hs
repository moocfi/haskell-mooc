{-# LANGUAGE TemplateHaskell #-}
module Set9bTest where

import Mooc.Test
import Mooc.Th

import Data.List
import qualified Data.Set as S
import Test.QuickCheck

import Set9b

main = score tests

tests = [ (1, "warmup",         [ ex1_nextRow, ex1_nextCol ])
        , (2, "prettyPrint",    [ ex2_examples, ex2_size, ex2_content, ex2_comm
                                , ex2_correctness ])
        , (3, "relations",      [ ex3_sameRow_pos, ex3_sameRow_neg
                                , ex3_sameCol_pos, ex3_sameCol_neg
                                , ex3_sameDiag_pos, ex3_sameDiag_neg
                                , ex3_sameAntidiag_pos, ex3_sameAntidiag_neg ])
        , (4, "danger",         [ ex4_danger, ex4_danger_neg, ex4_danger_neg_2 ])
        , (5, "prettyPrint2",   [ ex5_size, ex5_content, ex5_comm
                                , ex5_correctness ])
        , (6, "fixFirst",       [ ex6_fixFirst_safeZone, ex6_fixFirst_dangerZone, ex6_fixFirst_outside ])
        , (7, "stackOps",       [ ex7_continue, ex7_backtrack ])
        , (8, "nqueens_step",   [ ex8_step_4, ex8_step_continue, ex8_step_backtrack ])
        , (9, "nqueens_finish", [ ex9_finish_small, ex9_finish_medium, ex9_finish_large ])
        ]

-- -- -- -- --

size :: Coord -> Size
size (i,j) = foldr max 0 [8, i, j]

coord :: Gen (Int, Int)
coord = do
  (Positive i, Positive j) <- arbitrary :: Gen (Positive Int, Positive Int)
  return (i, j)

boundedCoord :: Size -> Gen Coord
boundedCoord n = do
  i <- choose (1,n)
  j <- choose (1,n)
  return (i,j)

board :: Gen (Size, [Coord])
board = do
  n  <- choose (5,10)
  k  <- choose (1,10)
  xs <- vectorOf k (boundedCoord n)
  let xs' = nub xs
  return (n, xs')

dangerZone :: Size -> Coord -> [Coord]
dangerZone n (i,j) =
  let dis = [-i + 1 .. n - i]
      djs = [-j + 1 .. n - j]
  in  [ (i + di, j + dj)
      | di <- dis, dj <- djs
      , di == dj || di == -dj || di * dj == 0
      ]

safeZone :: Size -> Coord -> [Coord]
safeZone n (i,j) =
  let dis = [-i + 1 .. n - i]
      djs = [-j + 1 .. n - j]
  in  [ (i + di, j + dj)
      | di <- dis, dj <- djs
      , di /= dj, di /= -dj, di * dj /= 0
      ]

full :: Size -> [Coord]
full n = [(i,j) | i <- [1..n], j <- [1..n]]

queens :: Size -> Int -> Gen ([Coord], [Coord], [Coord])
queens n 0 = return ([], [], full n)
queens n k = do
  (qs, dz, sz) <- queens n (k - 1)
  case sz of
    [] -> return (qs, dz, sz)
    _  -> do q <- elements sz
             let qs' = q:qs
             let dz' = dz `union` dangerZone n q
             let sz' = sz `intersect` safeZone n q
             return (qs', dz', sz')

stack :: Gen Stack
stack = do
  cs <- listOf1 arbitrary :: Gen [(Positive Int, Positive Int)]
  return $ map (\(Positive i, Positive j) -> (i,j)) cs

--------------------------------------------------------------------------------

ex1_nextRow = property $ do
  (i,j) <- coord
  return $ $(testing [| nextRow (i,j) |]) (?== (i + 1, 1))

ex1_nextCol = property $ do
  (i,j) <- coord
  return $ $(testing [| nextCol (i,j) |]) (?== (i, j + 1))

--------------------------------------------------------------------------------

ex2_examples = conjoin [$(testing [|prettyPrint 3 [(1,1),(2,3),(3,2)]|]) (?=="Q..\n..Q\n.Q.\n")
                       ,$(testing [|prettyPrint 3 [(2,3),(1,1),(3,2)]|]) (?=="Q..\n..Q\n.Q.\n")
                       ,$(testing [|prettyPrint 3 [(1,3),(2,1),(3,2)]|]) (?=="..Q\nQ..\n.Q.\n")]

ex2_size = property $ do
  (n, xs) <- board
  let ys = lines $ prettyPrint n xs
  return $ $(testing [| prettyPrint n xs |]) $ \out ->
    let ys = lines out
    in conjoin [ counterexample "  number of lines in output" $
                 length ys ?== n
               , counterexample "  length of output lines" $
                 map length ys ?== replicate n n ]

ex2_content = property $ do
  (n, xs) <- board
  let ys = prettyPrint n xs
  return $ $(testing [| prettyPrint n xs |]) $ \out ->
    counterexample "  characters in output" $
    sort (nub out) ?== (if null xs then "\n." else "\n.Q")

ex2_comm = property $ do
  (n, xs) <- board
  xs' <- shuffle xs
  return $ counterexample
    ("prettyPrint " ++ show n ++ " " ++ show xs ++ " == " ++
     "prettyPrint " ++ show n ++ " " ++ show xs' ++
     "\n  Expected: True\n  Was: False")
    (prettyPrint n xs == prettyPrint n xs')

ex2_correctness = property $ do
  (n, xs) <- board
  let m  = S.fromList xs
  let ys = lines $ prettyPrint n xs
  let check _     []  = True
      check (i,j) ("":rows) = check (i + 1, 1) rows
      check (i,j) ((char:row):rows)
        | char == 'Q' = S.member (i,j) m && check (i, j + 1) (row:rows)
        | char == '.' = (not (S.member (i,j) m)) && check (i, j + 1) (row:rows)
        | otherwise   = False
  return $ counterexample
    ("prettyPrint " ++ show n ++ " " ++ show xs ++
     " didn't mark the queens and/or empty cells correctly.\n" ++
     "The return value was:\n  " ++ show (prettyPrint n xs))
    (check (1,1) ys)

--------------------------------------------------------------------------------

ex3_sameRow_pos = property $ do
  (i,j) <- coord
  let xs = [(i, j + k) | k <- [0..9]]
  x <- elements xs
  y <- elements xs
  return $ $(testing [| sameRow x y |]) (?== True)

ex3_sameRow_neg = property $ do
  (i,j) <- coord
  let xs = [(i + k, j) | k <- [1..10]]
  x <- elements xs
  return $ $(testing [| sameRow (i,j) x |]) (?== False)

ex3_sameCol_pos = property $ do
  (i,j) <- coord
  let xs = [(i + k, j) | k <- [0..9]]
  x <- elements xs
  y <- elements xs
  return $ $(testing [| sameCol x y |]) (?== True)

ex3_sameCol_neg = property $ do
  (i,j) <- coord
  let xs = [(i, j + k) | k <- [1..10]]
  x <- elements xs
  return $ $(testing [| sameCol (i,j) x |]) (?== False)

ex3_sameDiag_pos = property $ do
  (i,j) <- coord
  let xs = [(i + k, j + k) | k <- [0..9]]
  x <- elements xs
  y <- elements xs
  return $ $(testing [| sameDiag x y |]) (?== True)

ex3_sameDiag_neg = property $ do
  (i,j) <- coord
  let xs = [(i, j + k) | k <- [1..10]]
  x <- elements xs
  return $ $(testing [| sameDiag (i,j) x |]) (?== False)

ex3_sameAntidiag_pos = property $ do
  (i,j) <- coord
  let xs = [ (i - k, j + k) | k <- [0..min i 10] ]
  x <- elements xs
  y <- elements xs
  return $ $(testing [| sameAntidiag x y |]) (?== True)

ex3_sameAntidiag_neg = property $ do
  (i,j) <- coord
  let xs = [(i, j + k) | k <- [1..10]]
  x <- elements xs
  return $ $(testing [| sameAntidiag (i,j) x |]) (?== False)

--------------------------------------------------------------------------------

ex4_danger = property $ do
  (i,j) <- coord
  (i',j') <- coord
  let n = max (size (i,j)) (size (i',j'))
  let xs = dangerZone n (i,j)
  x <- elements xs
  return $ conjoin [$(testing [| danger x [(i,j)] |]) (?== True)
                   ,$(testing [| danger x [(i,j),(i',j')] |]) (?== True)
                   ,$(testing [| danger x [(i',j'),(i,j)] |]) (?== True)
                   ,$(testing [| danger x [(i',j),(i',j'),(i,j)] |]) (?== True)]

ex4_danger_neg = property $ do
  (i,j) <- coord
  let n = size (i,j)
  let xs = safeZone n (i,j)
  x <- elements xs
  return $ $(testing [| danger x [(i,j)] |]) (?== False)

ex4_danger_neg_2 = property $ do
  let n = 8
  (i,j) <- boundedCoord n
  (i',j') <- boundedCoord n
  let xs = intersect (safeZone n (i,j)) (safeZone n (i',j'))
  return $
    (not (null xs) ==>) $
    forAllBlind (elements xs) $ \x ->
    $(testing [| danger x [(i,j),(i',j')] |]) (?== False)

--------------------------------------------------------------------------------

ex5_size = property $ do
  n <- choose (5,10) :: Gen Int
  let k = n `div` 2
  (qs, _, _) <- queens n k
  return $ $(testing [| prettyPrint2 n qs |]) $ \out ->
    let ys = lines out
    in conjoin [ counterexample "  number of lines in output" $
                 length ys ?== n
               , counterexample "  length of output lines" $
                 map length ys ?== replicate n n ]

ex5_content = property $ do
  n <- choose (5,10) :: Gen Int
  let k = n `div` 2
  (qs, _, _) <- queens n k
  return $ $(testing [| prettyPrint2 n qs |]) $ \out ->
    counterexample "  characters in output" $
    sort (nub out) ?== (if null qs then "\n." else "\n#.Q")

ex5_comm = property $ do
  n <- choose (5,10) :: Gen Int
  let k = n `div` 2
  (qs, dz, _) <- queens n k
  qs' <- shuffle qs
  return $ counterexample
    ("prettyPrint2 " ++ show n ++ " " ++ show qs ++ " == " ++
     "prettyPrint2 " ++ show n ++ " " ++ show qs' ++
     "\n  Expected: True\n  Was: False")
    (prettyPrint2 n qs == prettyPrint2 n qs')

ex5_correctness = property $ do
  n <- choose (5,10) :: Gen Int
  let k = n `div` 2
  (qs, dz, _) <- queens n k
  let q  = S.fromList qs
  let d  = S.fromList dz
  let xs = lines $ prettyPrint2 n qs
  let check _     []  = True
      check (i,j) ("":rows) = check (i + 1, 1) rows
      check (i,j) ((char:row):rows)
        | char == 'Q' = S.member (i,j) q &&
                        check (i, j + 1) (row:rows)
        | char == '#' = not (S.member (i,j) q) &&
                        S.member (i,j) d &&
                        check (i, j + 1) (row:rows)
        | char == '.' = not (S.member (i,j) q) &&
                        not (S.member (i,j) d) &&
                        check (i, j + 1) (row:rows)
        | otherwise   = False
  return $ counterexample
    ("prettyPrint2 " ++ show n ++ " " ++ show qs ++
     " didn't mark the queens and/or empty cells correctly.\n" ++
     "The return value was:\n  " ++ show (prettyPrint2 n qs))
    (check (1,1) xs)

--------------------------------------------------------------------------------

ex6_fixFirst_safeZone = property $ do
  n <- choose (5,10) :: Gen Int
  let k = n `div` 2
  (qs, dz, sz) <- queens n k
  case sz of
    [] -> return $ property True
    _  -> do q <- elements sz
             return $ $(testing [| fixFirst n (q:qs) |]) (?== Just (q:qs))

ex6_fixFirst_dangerZone = property $ do
  n <- choose (5,10) :: Gen Int
  let k = n `div` 2
  (qs, dz, sz) <- queens n k
  q@(i,j) <- elements dz
  return $ $(testing [| fixFirst n (q:qs) |]) . was $ \x -> case x of
    x@(Just (q'@(qi,qj):qs')) -> conjoin [counterexample
                                          ("  but " ++ show q' ++ " is on the wrong row")
                                          (qi ?== i)
                                         ,counterexample
                                          ("  but " ++ show q' ++ " is in danger")
                                          (q' `elem` sz)
                                         ,counterexample
                                          ("  but " ++ show qs' ++ " was not " ++ show qs)
                                          (qs' == qs)]
    x@(Just []) -> counterexample ("  but the list was empty") False
    Nothing -> counterexample
      ("  but there were safe squares on row " ++ show i ++
      ": " ++ show (intersect row sz))
      (all (`elem` dz) row)
      where row = [(i,k) | k <- [j .. n]]

ex6_fixFirst_outside = property $ do
  n <- choose (5,10)
  r <- choose (1,n)
  c <- choose (n+1,20)
  return $ $(testing [|fixFirst n [(r,c)]|]) (?==Nothing)

--------------------------------------------------------------------------------

ex7_continue = property $ do
  cs <- stack
  let input = show cs
  case continue cs of
    []  -> return $ counterexample
           ("continue " ++ input ++ " returned []")
           False
    cs' -> return $ conjoin
           [ counterexample
             ("length (continue " ++ input ++ ")")
             (length cs' ?== length cs + 1)
           , counterexample
             ("fst (head (continue " ++ input ++ "))")
             (fst (head cs') ?== fst (head cs) + 1)
           , counterexample
             ("snd (head (continue " ++ input ++ ")) should be positive, but \
              \it was " ++ show (snd (head cs')))
             (snd (head cs') >= 1)
           , counterexample
             ("tail (continue " ++ input ++ ")")
             (tail cs' ?== cs)
           ]

ex7_backtrack = property $ do
  cs <- stack `suchThat` ((>1).length)
  let input = show cs
  case backtrack cs of
    []  -> return $ counterexample
           ("backtrack " ++ input ++ " returned []")
           False
    cs' -> return $ conjoin
           [ counterexample
             ("length (backtrack " ++ input ++ ")")
             (length cs' ?== length cs - 1)
           , counterexample
             ("(head (backtrack " ++ input ++ "))")
             ((head cs') ?== (fst (cs!!1), snd (cs!!1) + 1))
           , counterexample
             ("tail (backtrack " ++ input ++ ")")
             (tail cs' ?== tail (tail cs))
           ]

--------------------------------------------------------------------------------

steps_4 = [[(1,1)]
          ,[(2,1),(1,1)]
          ,[(3,1),(2,3),(1,1)]
          ,[(2,4),(1,1)]
          ,[(3,1),(2,4),(1,1)]
          ,[(4,1),(3,2),(2,4),(1,1)]
          ,[(3,3),(2,4),(1,1)]
          ,[(2,5),(1,1)]
          ,[(1,2)]
          ,[(2,1),(1,2)]
          ,[(3,1),(2,4),(1,2)]
          ,[(4,1),(3,1),(2,4),(1,2)]
          ,[(5,1),(4,3),(3,1),(2,4),(1,2)]]

ex8_step_4 = conjoin [$(testing [|step 4 from|]) (?==to) | (from,to) <- zip steps_4 (tail steps_4)]

ex8_step_continue = property $ do
  n <- choose (5,10)
  k <- choose (1,n-1)
  (qs, dz, sz) <- queens n k
  let row = succ (fst (head qs))
      d = map snd $ filter ((==row).fst) dz
      s = map snd $ filter ((==row).fst) sz
  return $ not (null s) ==> do
    safec <- elements s
    let priors = takeWhile (`elem`d) [safec-1,safec-2..]
    col <- elements (safec:priors)
    return $ $(testing [|step n ((row,col):qs)|]) . was $ \res ->
      conjoin [counterexample ("  length") (length res ?== length qs + 2)
              ,counterexample ("  index 1") ((res!!1) ?== (row,safec))
              ,counterexample ("  end of list") (drop 2 res ?== qs)]

ex8_step_backtrack = property $ do
  n <- choose (5,10)
  k <- choose (div n 2,n-1)
  (qs, dz, sz) <- queens n k
  let row = succ (fst (head qs))
      d = map snd $ filter ((==row).fst) dz
      end = takeWhile (`elem`d) [n,n-1..]
  return $ not (null end) ==> do
    col <- elements end
    return $ $(testing [|step n ((row,col):qs)|]) . was $ \res ->
      conjoin [counterexample ("  length") (length res ?== length qs)
              ,counterexample ("  first element") (head res ?== nextCol (head qs))
              ,counterexample ("  end of list") (tail res ?== tail qs)]


--------------------------------------------------------------------------------

m_finish_sol sol = forAllBlind (elements (init (tails sol))) $ \pre ->
  $(testing [|finish (length sol) pre|]) (?==sol)

ex9_finish_small = conjoin [$(testing [|finish 4 [(1,1)]|]) (?==[(4,3),(3,1),(2,4),(1,2)])
                           ,m_finish_sol [(4,2),(3,4),(2,1),(1,3)]
                           ,m_finish_sol [(4,2),(3,4),(2,1),(1,3)]
                           ,m_finish_sol [(5,4),(4,2),(3,5),(2,3),(1,1)]
                           ,m_finish_sol [(5,5),(4,3),(3,1),(2,4),(1,2)]
                           ,m_finish_sol [(5,5),(4,2),(3,4),(2,1),(1,3)]
                           ,m_finish_sol [(5,2),(4,5),(3,3),(2,1),(1,4)]
                           ,m_finish_sol [(5,3),(4,1),(3,4),(2,2),(1,5)]]

m_finish_n n = $(testing [|finish n [(1,1)]|]) . was $ \qs ->
  conjoin [ counterexample "  length of list" $ length qs ?== n
          , counterexample "  queens outside board" $ [] ==? [(i,j) | (i,j) <- qs, i<1 || i>n || j<1 || j>n]
          , let sz = foldr intersect (full n) . map (\q -> q : safeZone n q) $ qs
            in counterexample "  these queens can capture each other: " $ (qs \\ sz) ?== []
          ]

ex9_finish_medium = forAllBlind (choose (5,8)) m_finish_n
ex9_finish_large = forAllBlind (choose (9,13)) m_finish_n
