module Chapter7_Exercises where

func :: Ord a => a -> a -> Bool
func x y = x == y

-- Notice that by passing in the number 1, instead of resolving 'a' to Integer, it resolves it to polymorphic Num a. However Num does not have an instance of Ord. So how does the compiler know to resolve to Num a? The answer is that ALL subclasses of Num implement Ord, so even though Num does not implement Ord, the typechecker can still resolve to polymorphic Num instead of concrete Int / Integer etc.
-- Further, if Num implemented Ord, then the resolution would be simply Num a instead of (Ord a, Num a)
funcNum :: (Ord a, Num a) => a -> Bool
funcNum = func 1

-- Here since we provided 1.5, the type variable 'a' is resolved to polymorphic Fractional, since its subclasses Float and Double both implement Ord
funcFrac :: (Ord a, Fractional a) => a -> Bool
funcFrac = func 1.5

-- Here the type variable 'a' is resolved to concrete type Char
funcChar :: Char -> Bool
funcChar = func 'a'

tensDigit :: Integral a => a -> a
tensDigit x = xLast `mod` 10
  where xLast = x `div` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = xLast `rem` 10
  where xLast = x `quot` 10

tensDigit'' :: Integral a => a -> a
tensDigit'' x = mod (fst . divMod x $ 10) 10

tensDigit''' :: Integral a => a -> a
tensDigit''' x = rem (fst . quotRem x $ 10) 10

hunsD :: Integral a => a -> a
hunsD x = rem (fst . quotRem x $ 100) 10

foldBool :: a -> a -> Bool -> a
foldBool x y bool = case bool of
  True  -> x
  False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y bool
  | bool == True = x
  | otherwise = y

foldBool'' :: a -> a -> Bool -> a
foldBool'' x y bool = if bool then x else y

foldBool''' :: a -> a -> Bool -> a
foldBool''' x _ True  = x
foldBool''' _ y False = y

g :: (a -> b) -> (a, c) -> (b, c)
g f t = (f (fst t), snd t)

g' :: (a -> b) -> (a, c) -> (b, c)
g' f t = (f . fst $ t, snd t)

g'' :: (a -> b) -> (a, c) -> (b, c)
g'' f (a, c) = (f a, c)

roundTrip :: (Read a, Show a) => a -> a
roundTrip a = read (show a)

roundTrip' :: (Read a, Show a) => a -> a
roundTrip' a = read . show $ a

roundTrip'' :: (Show a, Read b) => a -> b
roundTrip'' a = read . show $ a

roundTrip''' :: (Show a, Read b) => a -> b
roundTrip''' a = read (show a)

-- BlahType does not derive Show, so print (FirstPossible BlahType) fails.
-- Question: Why is it not necessary to constrain a, b, c to (Eq, Show) if SumOfThree is deriving (Eq, Show)?
data BlahType = BlahType

data SumOfThree a b c =
  FirstPossible a |
  SecondPossible b |
  ThirdPossible c
  deriving (Eq, Show)

-- Parenthesizing in the below manner ((a -> b) -> (b -> c)) seems to imply a higher-order function which takes a function (a -> b) as input and returns another function (a -> c) as output. The 'composition' operator takes a function (b -> c) as input and returns the higher-order function as output, which can curry-in the second function (a -> b) to return the output function (a -> c)
composition :: (b -> c) -> ((a -> b) -> (a -> c))
composition x y z = x (y z)
