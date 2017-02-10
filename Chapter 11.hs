{-# LANGUAGE FlexibleInstances #-}

module Chapter11 where

data Doggies a =
  Husky a |
  Mastiff a
  deriving (Eq, Show)

data Price = Price Integer
  deriving (Eq, Show)

data Manufacturer =
  Mini |
  Mazda |
  Tata
  deriving (Eq, Show)

type Weight = Integer

data Airline =
  PapuAir |
  CatapultsRUs |
  TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle =
  Car Manufacturer Price |
  Plane Airline Weight
  deriving (Eq, Show)

miniCar = Car Mini (Price 14000)
mazdaCar = Car Mazda (Price 20000)
tataCar = Car Tata (Price 7000)
doge = Plane PapuAir 50000

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _           = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _         = undefined

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany x = x > 42

newtype Goats = Goats Char
  deriving (Eq, Show)

instance TooMany Goats where
  tooMany (Goats x) = x <= 'Z'

newtype GoatProduct = GProd (Int, String)
  deriving (Eq, Show)

data GoatProductType = GPT Int String Int
  deriving (Eq, Show)

instance TooMany GoatProduct where
  tooMany (GProd (i, _)) = i > 42

-- Below instance declaration uses FlexibleInstances pragma
instance TooMany (Int, Int) where
  tooMany (i1, i2) = i1 + i2 > 42

instance (Num a, Ord a) => TooMany (a, a) where
  tooMany (a1, a2) = a1 + a2 > 42

instance TooMany GoatProductType where
  tooMany (GPT i1 s i2) = i1 + length s + i2 > 42

instance Integral a => TooMany (a, a, a) where
  tooMany (a1, a2, a3) = a1 + a2 == a3

data Person = Person {
  name :: String,
  age  :: Int
} deriving (Eq, Show)

data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

data BookType =
  FictionBook Fiction |
  NonfictionBook Nonfiction
  deriving Show

data BookType2 =
  Fiction2 | Nonfiction2
  deriving Show

type AuthorName = String

data Author = Author AuthorName BookType
  deriving Show

data Author2 =
  Author2F Fiction AuthorName |
  Author2NF Nonfiction AuthorName
  deriving Show

data Author3 = Author3 BookType2 AuthorName

a :: Author
a = Author "abc" (FictionBook Fiction)

a' :: Author2
a' = Author2F Fiction "pqr"

a'' :: Author2
a'' = Author2NF Nonfiction "xyz"

--

data GuessWhat = Chickenbutt
  deriving (Eq, Show)

data Id a = MkId a
  deriving (Eq, Show)

data Product a b = Product a b
  deriving (Eq, Show)

data RecordProduct a b = RecordProduct {
  pFirst  :: a,
  pSecond :: b
} deriving (Eq, Show)

data Sum a b = First a | Second b
  deriving (Eq, Show)

newtype NumCow = NumCow Int
  deriving (Eq, Show)

newtype NumPig = NumPig Int
  deriving (Eq, Show)

newtype NumSheep = NumSheep Int
  deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig
  deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep
  deriving (Eq, Show)

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool
type PoundsOfWool = Int

data CowInfo = CowInfo Name Age
  deriving (Eq, Show)

data PigInfo = PigInfo Name Age LovesMud
  deriving (Eq, Show)

data SheepInfo = SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)

data Animal =
  Cow CowInfo |
  Pig PigInfo |
  Sheep SheepInfo
  deriving (Eq, Show)

type Animal' =
  Sum CowInfo (Sum PigInfo SheepInfo)

bess = First (CowInfo "Bess" 4) :: Animal'
elmer' = Second (SheepInfo "Elmer" 5 5)
elmer = Second elmer' :: Animal'
elmo' = First (PigInfo "Elmo" 6 True)
elmo = Second elmo' :: Animal'

--

data OperatingSystem =
  GNULinux |
  OpenBSD |
  Mac |
  Windows
  deriving (Eq, Show)

data ProgrammingLanguage =
  Haskell |
  Agda |
  Idris |
  PureScript
  deriving (Eq, Show)

data Programmer = Programmer {
  os   :: OperatingSystem,
  lang :: ProgrammingLanguage
} deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GNULinux, OpenBSD, Mac, Windows]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer o l | o <- allOperatingSystems, l <- allLanguages]

--

data RecordCar = RecordCar {
  make  :: String,
  model :: String,
  year  :: Integer
} deriving (Eq, Show)

data Automobile = Null | Automobile RecordCar
  deriving (Eq, Show)

data Automobile' = Null' | RecordCar' {
  make'  :: String,
  model' :: String,
  year'  :: Integer
} deriving (Eq, Show)

--

data Quantum = Yes | No | Both
  deriving (Eq, Show)

convert1 :: Quantum -> Bool
convert1 Yes  = True
convert1 No   = True
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes  = True
convert2 No   = True
convert2 Both = False

convert3 :: Quantum -> Bool
convert3 Yes  = True
convert3 No   = False
convert3 Both = True

convert4 :: Quantum -> Bool
convert4 Yes  = False
convert4 No   = True
convert4 Both = True

convert5 :: Quantum -> Bool
convert5 Yes  = True
convert5 No   = False
convert5 Both = False

convert6 :: Quantum -> Bool
convert6 Yes  = False
convert6 No   = True
convert6 Both = False

convert7 :: Quantum -> Bool
convert7 Yes  = False
convert7 No   = False
convert7 Both = True

convert8 :: Quantum -> Bool
convert8 Yes  = False
convert8 No   = False
convert8 Both = False

--

data Quad = One | Two | Three | Four
  deriving (Eq, Show)

eQuad1 :: Either Quad Quad
eQuad1 = Left One

eQuad2 :: Either Quad Quad
eQuad2 = Left Two

eQuad3 :: Either Quad Quad
eQuad3 = Left Three

eQuad4 :: Either Quad Quad
eQuad4 = Left Four

eQuad5 :: Either Quad Quad
eQuad5 = Right One

eQuad6 :: Either Quad Quad
eQuad6 = Right Two

eQuad7 :: Either Quad Quad
eQuad7 = Right Three

eQuad8 :: Either Quad Quad
eQuad8 = Right Four

--

data Silly a b c d = MkSilly a b c d
  deriving (Eq, Show)

-- Any operator that starts with a colon (:) must be an infix type or data constructor.
-- All infix data constructors must start with a colon.
-- The type constructor of functions, (->), is the only infix type constructor that doesn’t start with a colon.
-- Another exception is that they cannot be :: as this syntax is reserved for type assertions.

data ProductInfix a b = a :&: b
  deriving (Eq, Show)
