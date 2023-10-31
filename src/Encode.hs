module Encode where

import Convert (splitByN, stringToBinaryString, bitsToString, toBinary, addHeadingZeroes)

import Data.List (nub)

getMinOutputClass uniqueChunks = ceiling (logBase 2 (fromIntegral . length $ uniqueChunks))

makeDictionary :: [String] -> [String] -> [(String, String)]
makeDictionary [] [] = []
makeDictionary [] _ = []
makeDictionary _ [] = []
makeDictionary (chunk:uniqueChunks) (key:keys) =
  (chunk, key) : makeDictionary uniqueChunks keys

encode :: String -> Int -> Int -> [String]
encode str inputClass outputClass = do
  let bitChunks = splitByN (stringToBinaryString str) inputClass
  let uniqueChunks = nub bitChunks
  if outputClass > getMinOutputClass uniqueChunks
    then do
      let encodeSequence = map (tail . bitsToString . toBinary) [0 .. (length uniqueChunks)]
      let outputWords = map (`addHeadingZeroes` outputClass) encodeSequence
      let dictionary = makeDictionary uniqueChunks outputWords
      outputWords
    else []
