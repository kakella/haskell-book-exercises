{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

w = y * 10
  where
    x = 5
    y = x + 5

x = 5
y = x + 5
f = 4 / y

f' = x ++ y ++ z
  where
    x = "Julie"
    y = " <3 "
    z = "Haskell"

bigNum = (^) 5 $ 10
-- check :type bigNum $ 10
-- bigNum $ 10 :: (Num (t -> t1), Num t) => t1
-- however bigNum $ 10 gives compilation error
-- do not understand this yet
-- if MonomorphismRestriction is enabled, then it gives Integer

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"
