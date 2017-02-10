module Chapter11_Exercises where

import           Data.Char (isAsciiUpper, toLower, toUpper)
import           Data.List (sort)

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf ls1 ls2 = and (f ls1 ls2 [])
  where
    f [] _ ol      = ol
    f (l:l1) l2 ol = f l1 l2 (elem l l2 : ol)

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

capitalizeWord :: String -> String
capitalizeWord ""       = ""
capitalizeWord (w:word)
  | w == ' ' = ' ' : capitalizeWord word
  | otherwise = toUpper w : word

capitalizeWords :: String -> [(String, String)]
capitalizeWords sentence = reverse (f (splitWords " " sentence) [])
  where
    f [] ol     = ol
    f (s:sl) ol = f sl ((s, capitalizeWord s) : ol)

capitalizeParagraph :: String -> String
capitalizeParagraph para = concatMap ((++ ".") . capitalizeWord) (splitWords "." para)

--

data Key =
  Zero [Char] |
  One [Char] |
  Two [Char] |
  Three [Char] |
  Four [Char] |
  Five [Char] |
  Six [Char] |
  Seven [Char] |
  Eight [Char] |
  Nine [Char] |
  Star [Char] |
  Hash [Char] |
  None
  deriving (Eq, Ord)

instance Show Key where
  show (Zero _)  = "Zero"
  show (One _)   = "One"
  show (Two _)   = "Two"
  show (Three _) = "Three"
  show (Four _)  = "Four"
  show (Five _)  = "Five"
  show (Six _)   = "Six"
  show (Seven _) = "Seven"
  show (Eight _) = "Eight"
  show (Nine _)  = "Nine"
  show (Star _)  = "Star"
  show (Hash _)  = "Hash"
  show None      = "None"

daPhone :: [Key]
daPhone = [
  Zero " 0+",
  One "1",
  Two "abc2",
  Three "def3",
  Four "ghi4",
  Five "jkl5",
  Six "mno6",
  Seven "pqrs7",
  Eight "tuv8",
  Nine "wxyz9",
  Star "*",
  Hash "#.,"
  ]

convo :: [String]
convo = [
  "Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol ok. Have u ever tasted alcohol lol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "Ok. Do u think I am pretty Lol",
  "Lol ya",
  "Haha thanks just making sure rofl ur turn"
  ]

getKey :: Char -> [Key] -> Key
getKey c phoneKeys = head (filter (elem (toLower c) . getKeyChar) phoneKeys)

getKeyChar :: Key -> [Char]
getKeyChar (Zero cs)  = cs
getKeyChar (One cs)   = cs
getKeyChar (Two cs)   = cs
getKeyChar (Three cs) = cs
getKeyChar (Four cs)  = cs
getKeyChar (Five cs)  = cs
getKeyChar (Six cs)   = cs
getKeyChar (Seven cs) = cs
getKeyChar (Eight cs) = cs
getKeyChar (Nine cs)  = cs
getKeyChar (Star cs)  = cs
getKeyChar (Hash cs)  = cs
getKeyChar _          = []

getDaPhoneKey :: Key -> [Key] -> Key
getDaPhoneKey key phoneKeys
  | not . null $ keyChar = getKey (head keyChar) phoneKeys
  | otherwise = None
  where
    keyChar = getKeyChar key

generateKeyPress :: Char -> [Key] -> [Key]
generateKeyPress char phoneKeys =
  if isAsciiUpper char
    then getKey '*' phoneKeys : f keyChars []
    else f keyChars []
  where
    key = getKey char phoneKeys
    keyChars = getKeyChar key
    f [] ol = ol
    f (c:cs) ol
      | c == toLower char = key : ol
      | otherwise = f cs (key : ol)

generateKeyPressSequence :: String -> [Key]
generateKeyPressSequence = concatMap (`generateKeyPress` daPhone)

generateConvo :: [String] -> [[Key]]
generateConvo = map generateKeyPressSequence

collapseSequence :: Eq a => [a] -> [(a, Int)]
collapseSequence = foldr f []
  where
    f a [] = [(a, 1)]
    f a ((token, count):ts) =
      if token == a
        then (token, count+1) : ts
        else (a, 1) : (token, count) : ts

countSequence :: [(a, Int)] -> Int
countSequence = foldr f 0
  where f (_, count) total = count + total

--

type Digit = Char
type Presses = Int
data PhoneKey = PhoneKey Digit [(Char, Presses)]
data Phone = Phone [PhoneKey]

phone :: Phone
phone = Phone [
  PhoneKey '0' [(' ', 1), ('0', 2), ('+', 3)],
  PhoneKey '1' [('1', 1)],
  PhoneKey '2' [('a', 1), ('b', 2), ('c', 3), ('2', 4)],
  PhoneKey '3' [('d', 1), ('e', 2), ('f', 3), ('3', 4)],
  PhoneKey '4' [('g', 1), ('h', 2), ('i', 3), ('4', 4)],
  PhoneKey '5' [('j', 1), ('k', 2), ('l', 3), ('5', 4)],
  PhoneKey '6' [('m', 1), ('n', 2), ('o', 3), ('6', 4)],
  PhoneKey '7' [('p', 1), ('q', 2), ('r', 3), ('s', 4), ('7', 5)],
  PhoneKey '8' [('t', 1), ('u', 2), ('v', 3), ('8', 4)],
  PhoneKey '9' [('w', 1), ('x', 2), ('y', 3), ('z', 4), ('9', 5)],
  PhoneKey '*' [],
  PhoneKey '#' [('#', 1), ('.', 2)]
  ]

reverseTaps :: Phone -> Char -> [(Digit, Presses)]
reverseTaps (Phone phones) = inner phones
  where
    f char = filter ((== char) . fst)
    inner [] _ = []
    inner (PhoneKey d cp : pk) c
      | null t = inner pk c
      | otherwise = [('*', 1) | isAsciiUpper c] ++ [(d, snd . head $ t)]
        where
          t = f (toLower c) cp

cellPhonesDead :: Phone -> String -> [(Digit, Presses)]
cellPhonesDead p = concatMap (reverseTaps p)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = countSequence

findMaxBySecondList :: Ord b => ([a], [b]) -> (a, b)
findMaxBySecondList ([], _) = undefined
findMaxBySecondList (_, []) = undefined
findMaxBySecondList (a':as', b':bs') = f as' bs' (a', b')
  where
    f [] [] ot = ot
    f [] _ ot = ot
    f _ [] ot = ot
    f (a:as) (b:bs) (at, bt)
      | b > bt = f as bs (a, b)
      | otherwise = f as bs (at, bt)

splitTuples :: [(a, b)] -> ([a], [b])
splitTuples ts' = f ts' ([], [])
  where
    f [] (as, bs)            = (reverse as, reverse bs)
    f ((a, b) : ts) (as, bs) = f ts (a:as, b:bs)

mostPopularLetter :: Ord a => [a] -> a
mostPopularLetter = fst . findMaxBySecondList . splitTuples . collapseSequence . sort

testString :: String
testString = "abcdeaceaea"

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord ls = mostPopularLetter (foldr ((++) . splitWords " .") [] ls)

--

data Expr =
  Lit Integer |
  Add Expr Expr

eval :: Expr -> Integer
eval (Lit i)     = i
eval (Add e1 e2) = eval e1 + eval e2

printExpr :: Expr -> String
printExpr (Lit i)     = show i
printExpr (Add e1 e2) = "(" ++ printExpr e1 ++ " + " ++ printExpr e2 ++ ")"

a1 :: Expr
a1 = Add (Lit 9001) (Lit 1)

a2 :: Expr
a2 = Add a1 (Lit 20001)

a3 :: Expr
a3 = Add (Lit 1) a2
