module Encode where

import Convert (splitByN, joinList, stringToBinaryString, bitsToString, toBinary, addHeadingZeroes)

import Data.List (nub)

getMinOutputClass uniqueChunks = ceiling $ logBase 2 $ fromIntegral . length $ uniqueChunks

makeDictionary :: [String] -> [String] -> [(String, String)]
makeDictionary [] [] = []
makeDictionary [] _ = []
makeDictionary _ [] = []
makeDictionary (chunk:uniqueChunks) (key:keys) =
  (chunk, key) : makeDictionary uniqueChunks keys

encode :: String -> Int -> Int -> (String, [(String, String)])
encode str inputClass outputClass = do
  let binaryChunks = splitByN (stringToBinaryString str) inputClass
  let uniqueChunks = nub binaryChunks
  if outputClass > getMinOutputClass uniqueChunks
    then do
      let encodeSequence = map (tail . bitsToString . toBinary) [0 .. (length uniqueChunks)]
      let outputWords = map (`addHeadingZeroes` outputClass) encodeSequence
      let dictionary = makeDictionary uniqueChunks outputWords
      (joinList . map (substitute dictionary) $ binaryChunks, dictionary)
    else ([], [])

substitute :: [(String, String)] -> String -> String
substitute (pair:dictionary) str
  | str == fst pair = snd pair
  | otherwise = substitute dictionary str
