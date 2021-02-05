module Examples.Bank where

import qualified Data.Map as Map

data Bank = Bank (Map.Map String Int)
  deriving (Show,Eq)

deposit :: String -> Int -> Bank -> Bank
deposit accountName amount (Bank accounts) =
  Bank (Map.adjust (\x -> x+amount) accountName accounts)

withdraw :: String -> Int -> Bank -> (Int,Bank)
withdraw accountName amount (Bank accounts) =
  let balance = Map.findWithDefault 0 accountName accounts  -- balance is 0 for a nonexistant account
      withdrawal = min amount balance                       -- can't withdraw over balance
      newAccounts = Map.adjust (\x -> x-withdrawal) accountName accounts
  in (withdrawal, Bank newAccounts)

-- `BankOp a` is an operation that transforms a Bank value, while returning a value of type `a`
data BankOp a = BankOp (Bank -> (a,Bank))

-- running a BankOp on a Bank
runBankOp :: BankOp a -> Bank -> (a,Bank)
runBankOp (BankOp f) bank = f bank

-- Running one BankOp after another
(+>>) :: BankOp a -> BankOp b -> BankOp b
op1 +>> op2 = BankOp combined
  where combined bank = let (_,bank1) = runBankOp op1 bank
                        in runBankOp op2 bank1

-- Running a parameterized BankOp, using the value returned by a previous BankOp
-- The implementation is a bit tricky but it's enough to understand how +> is used for now.
(+>) :: BankOp a -> (a -> BankOp b) -> BankOp b
op +> parameterized = BankOp combined
  where combined bank = let (a,bank1) = runBankOp op bank
                        in runBankOp (parameterized a) bank1

-- Make a BankOp out of deposit. There is no return value so we use ().
depositOp :: String -> Int -> BankOp ()
depositOp accountName amount = BankOp depositHelper
  where depositHelper bank = ((), deposit accountName amount bank)

-- Make a BankOp out of withdraw. Note how
--   withdraw accountName amount :: Bank -> (Int,Bank)
-- is almost a BankOp already!
withdrawOp :: String -> Int -> BankOp Int
withdrawOp accountName amount = BankOp (withdraw accountName amount)

-- distribute amount to two accounts
distributeOp :: String -> String -> Int -> BankOp ()
distributeOp to1 to2 amount =
  depositOp to1 half
  +>>
  depositOp to2 rest
  where half = div amount 2
        rest = amount - half

-- withdraw up to 100 units from one account, and distribute it evenly among two accounts
shareOp :: String -> String -> String -> BankOp ()
shareOp from to1 to2 =
  withdrawOp from 100
  +>
  distributeOp to1 to2
