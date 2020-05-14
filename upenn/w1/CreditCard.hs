module CreditCard where

toDigits      :: Integer -> [Integer]
toDigits 0 = []
toDigits n 
  | n > 0 = toDigits (div n 10) ++ [mod n 10]
  | otherwise = []


toDigitsRev   :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n 
  | n > 0 = mod n 10 : toDigitsRev (div n 10)
  | otherwise = []

  
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:zs)
  |  odd (length (x:y:zs))  =    x : y*2 : doubleEveryOther zs
  |  otherwise              =    x*2 : y : doubleEveryOther zs


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = x
sumDigits (x:y:zs) = (sumDigits (toDigits x)) + (sumDigits (toDigits y)) + sumDigits zs


validate :: Integer -> Bool
validate n = mod (sumDigits (doubleEveryOther (toDigits n))) 10 == 0
