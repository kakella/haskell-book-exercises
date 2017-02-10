module Chapter4 where

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah

changeMood2 :: Mood -> Mood
changeMood2 Blah = Woot
changeMood2    _ = Blah

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x =
  if x < 0 then (-x)
  else x

ft :: (a,b) -> (c,d) -> ((b,d),(a,c))
ft t1 t2 = ((snd t1, snd t2), (fst t1, fst t2))

f :: String -> Int
f xs = length xs + 1

id :: a -> a
id x = x

fh :: [a] -> a
fh = head

fh' :: (,) a b -> a
fh' = fst

type Name = String
data Pet = Cat | Dog Name

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f1 f2 x = fst (f2 (f1 x))
