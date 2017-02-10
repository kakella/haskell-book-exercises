module Chapter7 where

bindExp :: Integer -> String
bindExp x =
  let z = x + 1 in
    let y = 5 in
      "the integer was: " ++ show x ++
      " and y was: " ++ show y ++
      " and z was: " ++ show z

bindExp2 :: Integer -> String
bindExp2 x =
  (let x = 10; y = 5 in
    "x: " ++ show x ++
    ", y: " ++ show y)
  ++ ", outside x: " ++ show x

mTh1 x y z = x * y * z

mTh2 x y = \z -> x * y * z

mTh3 x = \y -> \z -> x * y * z

mTh4 = \x -> \y -> \z -> x * y * z

addOneIfOdd n = case odd n of
  True  -> f n
  False -> n
  where f = \n -> n + 1

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip = \f -> \x -> \y -> f y x

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User =
  UnregisteredUser |
  RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = print "UnregisteredUser"
printUser (RegisteredUser
  (Username name)
  (AccountNumber account)) =
    print $ name ++ " " ++ show account

k :: (a, b) -> a
k (x, y) = x

k1 :: Num a => a
k1 = k ((4 - 1), 10)

k2 :: [Char]
k2 = k ("three", (1 + 2))

k3 :: Num a => a
k3 = k (3, True)

f' :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f' (a, b, c) (d, e, f) = ((a, d), (c, f))

funcZ x =
  case x + 1 == 1 of
    True  -> "AWESOME"
    False -> "wut"

functionC x y =
  case (x > y) of
    True  -> x
    False -> y

ifEvenAdd2 n =
  case (even n) of
    True  -> n + 2
    False -> n

nums x =
  case (compare x 0) of
    EQ -> 0
    LT -> -1
    GT -> 1

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where y = x / 100

pal :: Eq a => [a] -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise        = False

numbers :: (Num a, Ord a) => a -> a
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1

f :: Int -> [Int] -> Int
f z xs = foldr (+) z xs

-- Note that f'' and f are the same, however f'' does not specify its arguments in the function body, only in the type signature. The lack of arguments has no effect on the type signature.
f'' :: Int -> [Int] -> Int
f'' = foldr (+)

add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \ x -> x + 1

addOnePF :: Int -> Int
addOnePF = (1+)

testAdds :: IO ()
testAdds = do
  print (0 :: Int)
  print (add 1 0)
  print (addOne 0)
  print (addOnePF 0)
  print ((addOne . addOne) 0)
  print ((addOnePF . addOne) 0)
  print ((addOnePF . addOnePF) 0)
  print (negate (addOne 0))
  print (negate . addOne $ 0)
  print (addOne . addOne . addOne . negate . addOne $ 0)
