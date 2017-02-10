module Strings where

main :: IO ()
main = do
  putStrLn meet
  putStrLn greet
    where meet = "welcome!"
          greet = concat [myGreeting, " ", "Sagar"]

myGreeting :: String
myGreeting = "hello"

topLevelFunction :: Integer -> Integer
topLevelFunction x = x + woot + topLevelValue
  where woot :: Integer
        woot = 5

topLevelValue :: Integer
topLevelValue = 10

trial :: IO ()
trial = do
  putStrLn greeting
  printSecond
  where
    greeting = "Zarrr"
    printSecond = print greeting

exclamatize :: String -> String
exclamatize x = x ++ "!"

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

rvrs :: String
rvrs =
  let str = "Curry is awesome"
  in
    concat [
      take 7 $ drop 9 str,
      " ",
      take 2 $ drop 6 str,
      " ",
      take 5 str
    ]
