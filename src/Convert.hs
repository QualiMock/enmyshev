module Convert (stringToBinary) where

import Data.Char (ord)

toBinary :: Int -> [Int]
toBinary = go 8 []
  where
    go 0 acc _ = reverse acc
    go n acc x = go (n - 1) (bit : acc) x'
      where
        (x', bit) = x `divMod` 2

toUnicode :: String -> [Int]
toUnicode = map ord

stringToBits :: String -> [Int]
stringToBits = concatMap toBinary . toUnicode

bitsToString :: [Int] -> String
bitsToString [0] = "0"
bitsToString [1] = "1"
bitsToString ints =
  (show . head $ ints) ++ (bitsToString . tail $ ints)

stringToBinary :: String -> String
stringToBinary = bitsToString . stringToBits
