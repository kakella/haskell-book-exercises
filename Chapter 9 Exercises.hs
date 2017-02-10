module Chapter9_Exercises where

import           Data.Char
import           Data.Maybe

filterUpper :: String -> String
filterUpper = filter isUpper

capitalizeFirstLetter :: String -> String
capitalizeFirstLetter ""     = ""
capitalizeFirstLetter (x:xs) = toUpper x : xs

capitalize :: String -> String
capitalize ""     = ""
capitalize (x:xs) = toUpper x : capitalize xs

getCapsHead :: String -> Maybe Char
getCapsHead "" = Nothing
getCapsHead s  = Just . toUpper . head $ s

myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

myElem :: Eq a => a -> [a] -> Bool
myElem q = not . null . filter (== q)

myElem' :: Eq a => a -> [a] -> Bool
myElem' q = not . myAny (== q)

myReverse :: [a] -> [a]
myReverse lst = r lst []
  where
    r [] out     = out
    r (x:xs) out = r xs (x:out)

squish :: [[a]] -> [a]
squish []     = []
squish (l:ls) = l ++ squish ls

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []     = []
squishMap f (l:ls) = f l ++ squishMap f ls

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myCompareBy :: (a -> a -> Ordering) -> Ordering -> [a] -> Maybe a
myCompareBy _ _ [] = Nothing
myCompareBy comp lean (l:ls) = Just (innerFunc ls l)
  where
    innerFunc [] max     = max
    innerFunc (x:xs) max = innerFunc xs (getMax x max)
    getMax max query =
      if (comp query max == lean)
        then query
        else max

myMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMaximumBy comp lst = myCompareBy comp GT lst

myMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMinimumBy comp lst = myCompareBy comp LT lst

myMaximum :: Ord a => [a] -> a
myMaximum = fromJust . myMaximumBy compare

myMinimum :: Ord a => [a] -> a
myMinimum = fromJust . myMinimumBy compare
