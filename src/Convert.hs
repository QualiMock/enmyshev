module Convert (stringToInt,
                toBinary,
                toStringOfBinaries,
                dictionaryToString,
                splitByNBits,
                addTrailingZeroes,
                joinList,
                fromBinary) where

import Data.Word (Word8)
import Data.Bits
import Data.List (foldl')

stringToInt :: String -> Int
stringToInt str
  | not . any (`elem` ['0'..'9']) $ str = 0
  | otherwise = read str :: Int

toBinary :: Word8 -> [Bool]
toBinary x = reverse [testBit x i | i <- [0 .. finiteBitSize x - 1]]

fromBinary :: [Bool] -> Word8
fromBinary = foldl' (\acc b -> (acc `shiftL` 1) .|. fromBool b) 0
  where
    fromBool True = 1
    fromBool False = 0

toStringOfBinaries :: [Bool] -> String
toStringOfBinaries (x:xs)
  | null xs && x = "1"
  | null xs && not x = "0"
  | x = '1' : toStringOfBinaries xs
  | not x = '0' : toStringOfBinaries xs

dictionaryToString [] = []
dictionaryToString (pair:dictionary) = toStringOfBinaries (fst pair) ++ " " ++
                                       toStringOfBinaries (snd pair) ++ "\n" ++
                                       dictionaryToString dictionary

splitByNBits :: [Bool] -> Int -> ([[Bool]], Int)
splitByNBits list size
  | size <= 1 = ([], 0)
  | null list = ([], 0)
  | length list == size = ([list], 0)
  | length list < size = do
      let missing = size - length list `rem` size
      ([addTrailingZeroes list missing], 0)
  | otherwise = do
      let splet = splitByNBits (drop size list) size
      let trailing = snd splet
      (take size list : fst splet, trailing)

addTrailingZeroes :: [Bool] -> Int -> [Bool]
addTrailingZeroes list missing = list ++ replicate missing False

joinList :: [[a]] -> [a]
joinList [x] = x
joinList (x:xs) = x ++ joinList xs
