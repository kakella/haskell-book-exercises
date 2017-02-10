module Chapter10_Exercises where

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

tupler :: String -> String -> [(Char, Char, Char)]
tupler s v = [(s1, v', s2) | s1 <- s, v' <- v, s2 <- s]

tupler' :: String -> String -> [(Char, Char, Char)]
tupler' s v = [('p', v', s2) | v' <- v, s2 <- s]

tupler'' :: [String] -> [String] -> [(String, String, String)]
tupler'' l1 l2 = [(s1, s2, s3) | s1 <- l1, s2 <- l2, s3 <- l1]

seekritFunc :: Fractional a => String -> a
seekritFunc x =
  fromIntegral (sum (map length (words x))) /
  fromIntegral (length (words x))

myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

myOr' :: [Bool] -> Bool
myOr' = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = any (== x)

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = foldr ((||) . (== x)) False

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr ((++) . (\x -> [x | f x])) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f ls = head (foldr func [] ls)
  where
    func p1 []   = [p1]
    func p1 [p2] =
      if f p1 p2 == GT
        then [p1]
        else [p2]
    func p1 (x:xs) =
      if f p1 x == GT
        then func p1 xs
        else func x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f ls = head (foldr func [] ls)
  where
    func p1 []   = [p1]
    func p1 [p2] =
      if f p1 p2 == LT
        then [p1]
        else [p2]
    func p1 (x:xs) =
      if f p1 x == LT
        then func p1 xs
        else func x xs
