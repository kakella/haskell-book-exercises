module Cipher where

import           Data.Char

-- Do modulus but within a range of non-zero limits 'begin' and 'end'
-- Eg: rangeMod 32 126 30  = 124
--     rangeMod 32 126 40  =  40
--     rangeMod 32 126 130 =  36
rangeMod :: Integral a => a -> a -> a -> a
rangeMod begin end query
  | begin < 0 || end < 0 = -1
  | begin >= end = -1
  | query < begin = end - (begin - query)
  | begin <= query && query <= end = query
  | query > begin = begin + er
  | otherwise = -1
  where
    er = mod query end

-- We are using ASCII table from 32 to 126 only
caesarCipher :: Int -> String -> String
caesarCipher count = map (chr . rangeMod 32 126 . (count+) . ord)

unCaesarCipher :: Int -> String -> String
unCaesarCipher count = caesarCipher (-count)

vigenereCipher :: String -> String -> String
vigenereCipher keyword inputString = go keyword inputString
  where
    go _ "" = ""
    go "" input = go keyword input
    go (k:key) (i:input) = caesarCipher (ord k - ord 'A') (i : "") ++ go key input

unVigenereCipher :: String -> String -> String
unVigenereCipher keyword outputString = go keyword outputString
  where
    go _ "" = ""
    go "" input = go keyword input
    go (k:key) (i:input) = caesarCipher (ord 'A' - ord k) (i : "") ++ go key input
