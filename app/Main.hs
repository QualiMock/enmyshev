module Main where

-- import Encode (encode)
import Convert (stringToInt, stringToBinaryString, splitByN, takeLast, toBinary, addHeadingZeroes, bitsToString)

import System.Environment
import System.IO
import GHC.Read (list)
import Data.List (nub)

main = do
  {
    args <- getArgs;
    if not . checkArgs $ args then
      print "Insufficient arguments"
    else do
      let inputFile = head args
      let outputFile = args !! 1
      let inputClass = stringToInt $ args !! 2
      let outputClass = stringToInt $ last args
      if inputClass > 0 then do
             fileContents <- readFile inputFile
             let bitChunks = splitByN (stringToBinaryString fileContents) inputClass
             let uniqueChunks = nub bitChunks
             if outputClass > getMinOutputClass uniqueChunks then do
               let encodeSequence = map (tail . bitsToString . toBinary) [0..(length uniqueChunks)]
               let outputWords = map (`addHeadingZeroes` outputClass) encodeSequence
               let dictionary = makeDictionary uniqueChunks outputWords
               print dictionary
             else
               putStrLn ("Output class size " ++ show outputClass ++ " is to small to encode:\n\t" ++
                         inputFile ++ "\nwith input class size " ++ show inputClass)
        else print "Input class size should be more than 0"
      -- writeFile outputFile encryptedContents;
      -- print ("Encode " ++ inputFile ++ " -> " ++ outputFile)
  }

getTrailingZeroes binaryStream inputClass = inputClass - (length binaryStream `rem` inputClass)

getMinOutputClass uniqueChunks = ceiling (logBase 2 (fromIntegral . length $ uniqueChunks))

makeDictionary :: [String] -> [String] -> [(String, String)]
makeDictionary [] [] = []
makeDictionary [] _ = []
makeDictionary _ [] = []
makeDictionary (chunk:uniqueChunks) (key:keys) =
  (chunk, key) : makeDictionary uniqueChunks keys

checkArgs :: [String] -> Bool
checkArgs args
  | length args < 4 = False
  | otherwise = True
