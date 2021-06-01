module Examples.Phantom where

data EUR
data USD
data CHF

data Money currency = Money Double
  deriving (Show,Eq)

dollar :: Money USD
dollar = Money 1

twoEuros :: Money EUR
twoEuros = Money 2

scaleMoney :: Double -> Money currency -> Money currency
scaleMoney factor (Money a) = Money (factor * a)

addMoney :: Money currency -> Money currency -> Money currency
addMoney (Money a) (Money b) = Money (a+b)

addMoneyUnsafe :: Money x -> Money y -> Money z
addMoneyUnsafe (Money a) (Money b) = Money (a+b)

data Rate from to = Rate Double
  deriving (Show,Eq)

eurToUsd :: Rate EUR USD
eurToUsd = Rate 1.22

convert :: Rate from to -> Money from -> Money to
convert (Rate r) (Money a) = Money (r*a)

invert :: Rate from to -> Rate to from
invert (Rate r) = Rate (1/r)

convertUnsafe :: Rate from to -> Money x -> Money y
convertUnsafe (Rate r) (Money a) = Money (r*a)
