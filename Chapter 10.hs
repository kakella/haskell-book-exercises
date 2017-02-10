module Chapter10 where

import           Data.Time

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x b -> f x || b) False xs

-- Evaluation steps for foldl (flip (*)) 1 [1..3]
-- foldl (*) (1 : 2 : 3 : []) 1
-- (((1 * 1) * 2) * 3)

-- foldl ((++) . show) "" [1..5]
-- This does not work because it translates to:
-- (++ . show) "" 1
-- and 1 is a number, not a string
-- Either change to the following:
-- foldr ((++) . show) "" [1..5] which gives "12345"
-- or change to the following:
-- foldl (flip ((++) . show)) "" [1..5] which gives "54321"

-- (1) foldr const 'a' [1..5] does not work
-- (2) foldr const 0 "tacos" also does not work
-- The reason is in the type signatures of foldr and const:
-- foldr :: (a -> b -> b) -> b -> t a -> b
-- const :: a -> b -> a
-- foldr expects first function to return type 'b'
-- foldr also expects the base case to be of type 'b'
-- const returns type 'a', which is first parameter to const
-- In the case of (1), a :: Num, b :: Char
-- So const expects to return a :: Num
-- However foldr expects const to return b :: Char
-- First evaluation of const is 5, 'a', returning 5 which is not Char
-- In the case of (2), a :: Char, b :: Num
-- First evaluation of const is 's', 0, returning 's' which is not Num

-- foldl const 0 "burritos"
-- This works because:
-- foldl :: (b -> a -> b) -> b -> t a -> b
-- a :: Char, b :: Num
-- First evaluation of const is 0, 'b', returning 0 which is Num

data DatabaseItem =
  DbString String |
  DbNumber Integer |
  DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
              (fromGregorian 1911 5 1)
              (secondsToDiffTime 34123)),
    DbNumber 9001,
    DbString "Hello, World!",
    DbDate (UTCTime
              (fromGregorian 1921 5 1)
              (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate xs = foldr f [] xs
  where
    f (DbDate d) lst = d : lst
    f _ lst          = lst

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber xs = foldr f [] xs
  where
    f (DbNumber d) lst = d : lst
    f _ lst            = lst

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = foldr f initVal xs
  where
    f (DbDate d1) d2 = max d1 d2
    f _ d2           = d2
    initVal = UTCTime
              (fromGregorian 1900 1 1)
              (secondsToDiffTime 0)

mostRecent' :: [DatabaseItem] -> UTCTime
mostRecent' xs = foldr max initVal (filterDbDate xs)
  where
    initVal = UTCTime
              (fromGregorian 1900 1 1)
              (secondsToDiffTime 0)

sumDb :: [DatabaseItem] -> Integer
sumDb xs = foldr (+) 0 (filterDbNumber xs)

avgDb :: [DatabaseItem] -> Double
avgDb xs = fromIntegral (sumDb xs) / (fromIntegral . length $filterDbNumber xs)

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs
-- decent explanation of this expression at:
-- https://gist.github.com/lukleh/67cf0a78205d3f6bd2d9

fibs20 :: [Integer]
fibs20 = take 20 fibs

fibsUnder100 :: [Integer]
fibsUnder100 = takeWhile (<100) fibs

factorial :: [Integer]
factorial = scanl (*) 1 [1..]

-- The next 3 examples show catamorphisms.
-- Note that the function name follows the Data constructor name:
--  Bool catamorphism is bool
--  Maybe catamorphism is maybe
--  Either catamorphism is either

-- data Bool = False | True
-- bool :: a -> a -> Bool -> a

-- data Maybe a = Nothing | Just a
-- maybe :: b -> (a -> b) -> Maybe a -> b

-- data Either a b = Left a | Right b
-- either :: (a -> c) -> (b -> c) -> Either a b -> c
