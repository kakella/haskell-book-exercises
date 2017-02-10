module Chapter8 where

import           Data.List (intersperse)

-- Reducing algorithm, bad performance, slows down noticeably by around n = 25
fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- Growing algorithm, good performance, works instantly upto n = 25000, possibly more
fibonacci' :: Integral a => a -> a
fibonacci' n = if n > 0 then fibInner 0 1 (n - 1) else 1
  where
    fibInner _  p1 0   = p1
    fibInner p0 p1 pos = fibInner p1 (p0 + p1) (pos-1)

-- Functional fibonacci - zip
fibonacci'' :: Integral a => [a]
fibonacci'' = 1 : 1 : [a + b | (a, b) <- zip fibonacci'' (tail fibonacci'')]

-- Functional fibonacci - zipWith
fibonacci''' :: Integral a => [a]
fibonacci''' = 1 : 1 : zipWith (+) fibonacci''' (tail fibonacci''')

-- Functional fibonacci - scanl
fibonacci'''' :: Integral a => [a]
fibonacci'''' = 1 : scanl (+) 1 fibonacci''''

data DividedResult a = Result (a, a) | DividedByZero
  deriving Show

dividedBy :: Integral a => a -> a -> DividedResult a
dividedBy _   0     = DividedByZero
dividedBy num denom = Result (innerFunc (abs num) (abs denom) 0)
  where
    negify = if (num * denom) < 0 then -1 else 1
    innerFunc n d c
      | n < d     = (negify * c, n)
      | otherwise = innerFunc (n - d) d (c + 1)

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

sumNum :: (Eq a, Num a) => a -> a
sumNum n
  | n == 0    = 0
  | otherwise = n + sumNum (n - 1)

multiplyNum :: Integral a => a -> a -> a
multiplyNum 0  _  = 0
multiplyNum _  0  = 0
multiplyNum n1 n2 =
  if n1 < n2
    then func n2 n1
    else func n1 n2
  where
    func _   0     = 0
    func num count = num + func num (count - 1)

mcCarthy91 :: (Num a, Ord a) => a -> a
mcCarthy91 x
  | x > 100   = x - 10
  | otherwise = mcCarthy91 (mcCarthy91 (x + 11))

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = ""

digits :: Int -> [Int]
digits n = getAllDigits n []
  where
    getLastDigit d = d `mod` 10
    getButLastDigit d = d `div` 10
    getAllDigits d lst =
      let
        oLst = getLastDigit d : lst
        remDigits = getButLastDigit d
       in
        if remDigits /= 0
          then getAllDigits remDigits oLst
          else oLst

wordNumber :: Int -> String
wordNumber n = concat . intersperse "-" . map digitToWord . digits $ n
