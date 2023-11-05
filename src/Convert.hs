module Convert (stringToBinaryString,
                stringToInt,
                toBinary,
                bitsToString,
                splitByN,
                joinList,
                addTrailingZeroes,
                addHeadingZeroes,
                takeLast) where

import Data.Char (ord)
import Data.Foldable (Foldable(foldl'))

toBinary 0 = [0]
toBinary n
  | odd n  = toBinary (n `div` 2) ++ [1]
  | even n = toBinary (n `div` 2) ++ [0]

toUnicode = map ord

stringToBits = concatMap toBinary . toUnicode

bitsToString :: [Int] -> String
bitsToString [0] = "0"
bitsToString [1] = "1"
bitsToString ints =
  (show . head $ ints) ++ (bitsToString . tail $ ints)

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

joinList :: [String] -> String
joinList [x] = x
joinList (x:xs) = x ++ joinList xs

addTrailingZeroes list missing = list ++ replicate missing '0'

addHeadingZeroes list size = replicate (size - length list) '0' ++ list

takeLast :: Int -> [a] -> [a]
takeLast n xs = foldl' (const . drop 1) xs (drop n xs)
