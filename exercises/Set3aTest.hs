{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Set3aTest where

--import Control.Monad
import Data.Char
import Data.List
import Test.QuickCheck

import Mooc.Th
import Mooc.Test

import Set3a

main = score tests

tests = [(1,"maxBy",[ex1_maxBy])
        ,(2,"mapMaybe",[ex2_mapMaybe_Nothing, ex2_mapMaybe_Just_String, ex2_mapMaybe_Just_Int])
        ,(3,"mapMaybe2",[ex3_mapMaybe2])
        ,(4,"palindromeHalfs",[ex4_palindromeHalfs])
        ,(5,"capitalize",[ex5_capitalize_1, ex5_capitalize_2])
        ,(6,"powers",[ex6_powers_small, ex6_powers_large])
        ,(7,"while",[ex7_while_number, ex7_while_string])
        ,(8,"whileRight",[ex8_whileRight_Left, ex8_whileRight_step])
        ,(9,"joinToLength",[ex9_1])
        ,(10,"+|+",[ex10])
        ,(11,"sumRights",[ex11])
        ,(12,"multiCompose",[ex12_empty, ex12_scalability, ex12_arithmetic, ex12_strings])
        ,(13,"multiApp",[ex13_empty, ex13_scalability, ex13_arithmetic, ex13_strings, ex13_mixed])
        ,(14,"interpreter",[ex14_interpreter_1, ex14_interpreter_2])
        ]

-- -- -- -- -- --

letter = choose ('a','z')
word = listOf1 letter

ex1_maxBy = property $ do
  t <- choose (0,20)
  f <- choose (0,20) `suchThat` \f -> f/=t
  let p True = t
      p False = f
  return $
    counterexample ("let p True = "++show t++"; p False = "++show f++" in maxBy p False True") $
    maxBy p False True ?== (t>f)

ex2_mapMaybe_Nothing =
  $(testing' [|mapMaybe negate Nothing|]) (?==Nothing)

ex2_mapMaybe_Just_String = forAllBlind word $ \w ->
  counterexample ("mapMaybe length (Just " ++ show w ++ ")") $
  mapMaybe length (Just w) ?== Just (length w)

ex2_mapMaybe_Just_Int = forAll_ $ \(i::Int) ->
  counterexample ("mapMaybe negate (Just " ++ show i ++ ")") $
  mapMaybe negate (Just i) ?== Just (negate i)

ex3_mapMaybe2 = forAll_ $ \(Positive i,Positive j) ->
  let t :: Maybe Integer -> Maybe Integer -> Maybe Integer -> Property
      t a b exp = counterexample ("mapMaybe div " ++ show' a ++ " " ++ show' b) $ mapMaybe2 div a b ?== exp
  in conjoin [t (Just i) (Just j) (Just (div i j))
             ,t Nothing (Just j) Nothing
             ,t (Just i) Nothing Nothing
             ,t Nothing Nothing Nothing]

unpalindrome = word `suchThat` \w -> w /= reverse w

ex4_palindromeHalfs = property $ do
  half1 <- word
  let palindrome1 = half1 ++ reverse half1
  half2 <- word
  let palindrome2 = half2 ++ reverse (init half2)
  un1 <- listOf1 unpalindrome
  un2 <- listOf1 unpalindrome
  un3 <- listOf1 unpalindrome
  let input = un1 ++ [palindrome1] ++ un2 ++ [palindrome2] ++ un3
  let expected = [half1, half2]
  let palindromeHalfs xs = map firstHalf (filter palindrome xs)
  return $ $(testing [|palindromeHalfs input|]) (?==expected)

ex5_capitalize_1 = property $ do
  cap <- letter
  w1 <- word
  w2 <- word
  return $ $(testing [|capitalize (cap:w1++" "++cap:w2)|]) (?==(toUpper cap:w1++" "++toUpper cap:w2))

ex5_capitalize_2 = property $ do
  ws <- listOf1 word
  let input = unwords ws
  let positions = init $ scanl (+) 0 $ map (succ.length) ws
  p <- elements positions
  q <- elements [0..length input - 1]
  return . $(testing [|capitalize input|]) $ \v ->
    conjoin [counterexample ("char at index "++show p) $ v !! p ?== toUpper (input !! p)
            ,not (elem q positions) ==> counterexample ("char at index "++show q) (v !! q ?== input !! q)]

m_powers maxlen = property $ do
  n <- choose (2,5)
  len <- choose (1,maxlen)
  end <- choose (n^(len-1),n^len-1)
  return $ $(testing [|powers n end|]) $ \p -> conjoin
    [counterexample "all smaller than end" $ all (<=end) p
    ,counterexample "in ascending order" $ p == sort p
    ,counterexample "length" $ length p ?== len
    ,counterexample "all powers of n" $ all (check n) p]
  where check n 0 = True
        check n 1 = True
        check n k
          | k `mod` n == 0    = check n (div k n)
          | otherwise         = False

ex6_powers_small = m_powers 5
ex6_powers_large = m_powers 27 -- 5^27 still fits in Int

ex7_while_number = property $ do
  n <- choose (0,20 :: Integer)
  return $ counterexample ("while (/="++show n++") (+1) 0") $ within timeLimit $
    while (/=n) (+1) 0 ?== n

ex7_while_string = property $ do
  n <- word
  let w = n++n
      p = (/=n)
  return $ counterexample ("while (/="++show n++") tail "++show w) $ within timeLimit $
    while p tail w == n

ex8_whileRight_Left = forAllBlind word $ \w ->
  forAllBlind (choose (1::Int,10)) $ \i ->
  counterexample ("let f _ = Left " ++ show w ++ " in whileRight f " ++ show i) $ within timeLimit $
  let f _ = Left w in whileRight f i ?== w

ex8_whileRight_step = property $ do
  let step :: Int -> Int -> Either Int Int
      step k x = if x<k then Right (2*x) else Left x
  limit <- choose (1,1000)
  start <- elements [1,3,5]
  let log = ceiling . logBase 2 . fromIntegral
      divUp x y = ceiling (fromIntegral x / fromIntegral y)
      answer = start * 2 ^ (log (limit `divUp` start))
  return $ counterexample ("whileRight (step " ++ show limit ++ ") " ++ show start) $ within timeLimit $
    whileRight (step limit) start ?== answer

ex9_1 = property $ do
  w <- word
  v <- word `suchThat` diffLength w
  let target = length w + length v
  cruft <- listOf1 word
  let input = sort $
              ([w,v]++) $
              nubBy (\x y -> length x + length y == target) $
              filter (\x -> 2 * length x /= target) $
              filter (\x -> diffLength x v && diffLength x w) $
              cruft
      output = [w++v, v++w]
  return $ $(testing [|joinToLength target input|]) (hasElements output)
  where diffLength a b = length a /= length b

ex10 = $(withDefined "+|+") $ \op ->
  forAll_ $ \(x::Int,y::Int,xs'::[Int],ys'::[Int]) ->
  let xs = x:xs'
      ys = y:ys'
  in conjoin [counterexample (show xs ++ " +|+ " ++ show ys) $ op xs ys ?== [x,y]
             ,counterexample (show xs ++ " +|+ []") $ op xs [] ?== [x]
             ,counterexample ("[] +|+ " ++ show xs) $ op [] xs ?== [x]]

ex11 = property $ do
  is <- listOf (choose (0,10))
  es <- listOf word
  input <- shuffle (map Right is ++ map Left es)
  return $ $(testing [|sumRights input|]) (?==sum is)

ex12_empty = property $ do
  n <- choose (0,9) :: Gen Int
  let input  = multiCompose [] n
      output = n
  return $ counterexample ("multiCompose [] " ++ show n)
         $ input ?== output

ex12_scalability = property $ do
  n <- choose (0,9) :: Gen Int
  let input  = multiCompose (replicate n (succ::Int->Int)) 0
      output = n
  return $ counterexample ("multiCompose (replicate " ++ show n ++ " succ) 0")
         $ input ?== output

ex12_arithmetic = property $ do
  n  <- choose (0,5) :: Gen Int
  let input  = multiCompose [(3*), (^2), (+1)] n
      output = ((3*) . (^2) . (+1)) n
  return $ counterexample ("multiCompose [(3*), (^2), (+1)] " ++ show n)
         $ input ?== output

ex12_strings = property $ do
  s <- listOf letter `suchThat` (\t -> length t > 1)
  let input  = multiCompose [(++"xy"), reverse, tail, reverse] s
      output = (++"xy") . reverse . tail . reverse $ s
  return $ counterexample ("multiCompose [(++\"xy\"), reverse, tail, reverse] " ++ show s)
         $ input ?== output

ex13_empty = property $ do
  n <- choose (0,9) :: Gen Int
  let input  = multiApp id [] n
      output = [] :: [Int]
  return $ counterexample ("multiApp id [] " ++ show n)
         $ input ?== output

ex13_scalability = property $ do
  n <- choose (0,9) :: Gen Int
  k <- choose (0,9) :: Gen Int
  let input  = multiApp (sum::[Int]->Int) (replicate n (succ::Int->Int)) k
      output = n*(k+1)
  return $ counterexample ("multiApp sum (replicate " ++ show n ++ " succ) " ++ show k)
         $ input ?== output

ex13_arithmetic = property $ do
  n  <- choose (0,9) :: Gen Int
  let gs     = [(+1), (*2), (`div`2), (^2)]
      input  = multiApp (sum::[Int]->Int) gs n
      output = (n + 1) + (n * 2) + (n `div` 2) + n^2
  return $ counterexample ("multiApp sum [(+1), (*2), (`div`2), (^2)]" ++ show n)
         $ input ?== output

ex13_strings = property $ do
  words <- vectorOf 3 word
  let gs     = [(\xs -> [head xs]), tail, reverse]
      input  = multiApp id gs words
      output = [[head words], tail words, reverse words]
  return $ counterexample ("multiApp id [(\\xs -> [head xs]), tail, reverse]" ++ show words)
         $ input ?== output

ex13_mixed = counterexample ("multiApp sum [head, last] [1,2,3,4]") $
             multiApp (sum::[Int]->Int) [head, last] [1::Int,2,3,4] ?== 5

ex14_interpreter_1 = property $ do
  up <- choose (0,10)
  right <- choose (0,10)
  down <- choose (1,3)
  left <- choose (0,1)

  let first = replicate up "up" ++ replicate right "right" ++ ["printY","printX"]
      second = replicate down "down" ++ replicate left "left" ++ ["printY","printX"]
      input = first ++ second
      output = [show up, show right, show (up-down), show (right-left)]

  return $ counterexample ("interpreter "++show input) $
    interpreter input ?== output

ex14_interpreter_2 = property $ do
  nums <- vectorOf 4 $ choose (0,10)
  let diffs = zipWith (-) nums (0:nums)
      f x | x<0 = replicate (negate x) "down"
          | otherwise = replicate x "up"
      input = concatMap (\x -> f x ++ ["printY"]) diffs
      output = map show nums
  return $ $(testing [|interpreter input|]) (?== output)
