module Encode (encode, addMissingZeroes) where
import Convert (stringToBinary)

encode :: String -> Int -> Int -> [String]
encode str inputClass outputClass = splitByN str inputClass

splitByN :: String -> Int -> [String]
splitByN str size
  | size <= 1 = []
  | null str = []
  | length str == size = [str]
  | length str < size = [addMissingZeroes str (size - length str `rem` size)]
  | otherwise = take size str : splitByN (drop size str) size

addMissingZeroes :: String -> Int -> String
addMissingZeroes str missing = str ++ replicate missing '0'
