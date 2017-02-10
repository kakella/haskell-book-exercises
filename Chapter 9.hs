module Chapter9 where

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail [_]    = Nothing
safeTail (_:xs) = Just xs

eftInt :: Int -> Int -> [Int]
eftInt start end
  | start > end = []
  | otherwise = start : eftInt (succ start) end

mySplitStrings :: String -> Char -> [String]
mySplitStrings s delimiter = getNextWord s []
  where
    firstWord = takeWhile (/= delimiter)
    remaining = dropWhile (/= delimiter)
    removeSpace = dropWhile (== delimiter)

    getNextWord "" lst = reverse lst
    getNextWord str lst = getNextWord (removeSpace $ remaining str) (firstWord str : lst)

myWords :: String -> [String]
myWords s = mySplitStrings s ' '

myLines :: String -> [String]
myLines s = mySplitStrings s '\n'

filter3Multiple :: [Integer] -> [Integer]
filter3Multiple [] = []
filter3Multiple (x:xs)
  | x `rem` 3 == 0 = x : filter3Multiple xs
  | otherwise = filter3Multiple xs

myFilter :: String -> [String]
myFilter xs = filter (not . f) (words xs)
  where f = flip elem ["a", "an", "the"]

myZip :: [a] -> [b] -> [(a, b)]
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys
myZip _ _           = []

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys
myZipWith _ _ _           = []

myZip' :: [a] -> [b] -> [(a, b)]
myZip' = myZipWith (,)
