module Money (
  Money
  , cents
  , dollars
  , amount
  , fromCents
  , fromDollars
  , ($+$), ($+), (+$)
  , ($-$), ($-), (-$)
  , ($/$)
  , ($*), (*$), ($/), (/$)
  ) where

import Control.Arrow (second)

newtype Money = Money { unM :: Integer } deriving (Eq, Ord)

instance Show Money where
  show m = '$' : d ++ "." ++ c
    where d = show (dollars m)
          c | cents' < 10 = '0' : show cents'
            | otherwise = show cents'
          cents' = cents m

factor = 100

amount x = Money (dollars * factor + cents)
  where (dollars, cents) = second (round . (* fromIntegral factor)) $ properFraction x
fromCents = Money
fromDollars = Money . (* factor)

cents = (`rem` factor) . unM
dollars = (`div` factor) . unM        

a $+$ b = Money (unM a + unM b)
a $-$ b = Money (unM a - unM b)
a $/$ b = fromIntegral (unM a) / fromIntegral (unM b)
m $+ f = m $+$ amount f
m $- f = m $-$ amount f
(+$) = flip ($+)
(-$) = flip ($-)

scale fac m = Money (round cents')
  where cents' = fromIntegral (unM m) * fac
(*$) = scale
($*) = flip (*$)
(/$) = scale . recip
($/) = flip (/$)a