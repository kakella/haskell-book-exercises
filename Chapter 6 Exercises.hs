module Chapter6_Exercises where

import           Data.List

data Person = Person Bool
  deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn(show person)

data Mood = Blah | Woot
  deriving (Eq, Show)

settleDown x =
  if x == Woot
    then Blah
    else x

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"  -- s1 :: Object -> Sentence
s2 = Sentence "Julie" "loves" "dogs"

data Rocks = Rocks String
  deriving (Eq, Show, Ord)

data Yeah = Yeah Bool
  deriving (Eq, Show, Ord)

data Papu = Papu Rocks Yeah
  deriving (Eq, Show, Ord)

i :: Num a => a
i = 1

f :: RealFrac a => a
f = 1.0

freud :: Int -> Int
freud x = x

myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX

jung :: [Char] -> Char
jung xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk func a b = (func a) == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith func int a = (func a) ^ int
