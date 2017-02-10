module Chapter12_Exercises where

splitWords :: [Char] -> String -> [String]
splitWords delimiterList sentence = reverse (f sentence "" [])
  where
    f "" curr ol
      | curr == "" = ol
      | otherwise = reverse curr : ol
    f (d:ls) curr ol
      | elem d delimiterList && curr == "" = f ls "" ol
      | elem d delimiterList && curr /= "" = f ls "" (reverse curr : ol)
      | otherwise = f ls (d:curr) ol

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s     = Just s

replaceThe :: String -> String
replaceThe s = foldr (f . notThe) "" (splitWords " " s)
  where
    f Nothing x   = "a " ++ x
    f (Just a) "" = a
    f (Just a) x  = a ++ " " ++ x

vowels :: String
vowels = "aeiou"

isVowel :: Char -> Bool
isVowel c = c `elem` vowels

countTheBeforeVowel' :: [String] -> [Maybe String]
countTheBeforeVowel' [] = []
countTheBeforeVowel' (x:xs)
  | isVowel $ head x = Just "V" : countTheBeforeVowel' xs
  | x == "the" = Just "T" : countTheBeforeVowel' xs
  | otherwise = Nothing : countTheBeforeVowel' xs

countTheBeforeVowel :: String -> Int
countTheBeforeVowel s = f (countTheBeforeVowel' (splitWords " " s)) 0 False
  where
    f [] count _ = count
    f (Nothing:xs) count _ = f xs count False
    f (Just x:xs) count isVowelPred
      | x == "V" = f xs count True
      | x == "T" && isVowelPred = f xs (count+1) False
      | x == "T" && not isVowelPred = f xs count False
      | otherwise = f xs count False

returnVowelsInString :: String -> String
returnVowelsInString = filter isVowel

countVowels :: String -> Int
countVowels = length . returnVowelsInString

returnConsonantsInString :: String -> String
returnConsonantsInString = filter (not . isVowel)

countConsonants :: String -> Int
countConsonants = length . returnConsonantsInString

newtype Word' = Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s = if countVowels s > countConsonants s
  then Nothing
  else Just (Word' s)

data Nat = Zero | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | otherwise = Just (go i)
  where
    go 0 = Zero
    go x = Succ (go (x-1))

isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee base _ Nothing = base
mayybee _ f (Just a)   = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe base Nothing = base
fromMaybe _ (Just a)   = a

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

catMaybes' :: [Maybe a] -> [a]
catMaybes' []           = []
catMaybes' (Nothing:as) = catMaybes' as
catMaybes' (Just a:as)  = a : catMaybes' as

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe ls = if null expr
  then Nothing
  else Just expr
  where
    expr = f ls []
    f [] out          = reverse out
    f (Nothing:_) _   = []
    f (Just x:xs) out = f xs (x:out)

lefts' :: [Either a b] -> [a]
lefts' []           = []
lefts' (Left a:xs)  = a : lefts' xs
lefts' (Right _:xs) = lefts' xs

lefts'' :: [Either a b] -> [a]
lefts'' = foldr f []
  where
    f (Left a) xs  = a : xs
    f (Right _) xs = xs

rights'' :: [Either a b] -> [b]
rights'' = foldr f []
  where
    f (Left _) xs  = xs
    f (Right a) xs = a : xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' ls = (lefts'' ls, rights'' ls)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right b) = Just (f b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fl _ (Left a)  = fl a
either' _ fr (Right b) = fr b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' fr = either' fl (Just . fr)
  where fl _ = Nothing

myIterate :: (a -> a) -> a -> [a]
myIterate f initial = initial : myIterate f (f initial)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f initial = go f (f initial)
  where
    go :: (b -> Maybe (a, b)) -> Maybe (a, b) -> [a]
    go _ Nothing        = []
    go f' (Just (a, b)) = a : go f' (f' b)

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr f'
  where
    f' x = Just (x, f x)
