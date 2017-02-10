module Chapter6 where

data Trivial = Trivial' | Trivial''

instance Eq Trivial where
  Trivial'  == Trivial'  = True
  Trivial'  == Trivial'' = False
  Trivial'' == Trivial'  = False
  Trivial'' == Trivial'' = True

data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Ord, Show)

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==)   _   _ = False

data Date = Date DayOfWeek Int Int Int deriving Show

instance Eq Date where
  (==) (Date weekday dayOfMonth month year) (Date weekday' dayOfMonth' month' year') =
    weekday == weekday' &&
    dayOfMonth == dayOfMonth' &&
    month == month' &&
    year == year'

data Identity a = Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn y) = x == y

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two x y) (Two a b) = (x == a) && (y == b)

data StringOrInt =
  TisAnInt Int |
  TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt i) (TisAnInt i')     = i == i'
  (==) (TisAString s) (TisAString s') = s == s'
  (==) (TisAnInt _) (TisAString _)    = False
  (==) (TisAString _) (TisAnInt _)    = False

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = (x == x') && (y == y')

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = (x == x') && (y == y')

data Which a =
  ThisOne a |
  ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') = x == x'
  (==) (ThatOne y) (ThatOne y') = y == y'
  (==) (ThisOne _) (ThatOne _)  = False
  (==) (ThatOne _) (ThisOne _)  = False

data EitherOr a b =
  Hello a |
  Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello x')     = x == x'
  (==) (Goodbye y) (Goodbye y') = y == y'
  (==) (Hello _) (Goodbye _)    = False
  (==) (Goodbye _) (Hello _)    = False

instance Show (EitherOr a b) where
  show (Hello _)   = "Hello"
  show (Goodbye _) = "Goodbye"
