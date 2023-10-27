module Convert (stringToBinaryString, stringToInt, splitByN, addTrailingZeroes) where

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

stringToBinaryString :: String -> String
stringToBinaryString = bitsToString . stringToBits

stringToInt :: String -> Int
stringToInt str
  | not . any (`elem` ['0'..'9']) $ str = 0
  | otherwise = read str :: Int

splitByN :: String -> Int -> [String]
splitByN list size
  | size <= 1 = []
  | null list = []
  | length list == size = [list]
  | length list < size = [addTrailingZeroes list (size - length list `rem` size)]
  | otherwise = take size list : splitByN (drop size list) size

addTrailingZeroes :: String -> Int -> String
addTrailingZeroes list missing = list ++ replicate missing '0'
