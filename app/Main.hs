module Main where

import Encode (encode)
import Convert (stringToInt, stringToBits)

import Data.Char (ord)
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
             let encodedContents = encode fileContents inputClass outputClass;
             let dictionary = makeDictionary (nub encodedContents) [0..(length encodedContents)]
             mapM_ print dictionary
        else print "Input class size should be more than 0"
      -- writeFile outputFile encryptedContents;
      -- print ("Encode " ++ inputFile ++ " -> " ++ outputFile)
  }

getTrailingZeroes binaryStream inputClass = inputClass - (length binaryStream `rem` inputClass)

makeDictionary :: [[Int]] -> [Int] -> [([Int], Int)]
makeDictionary [] [] = []
makeDictionary [] _ = []
makeDictionary _ [] = []
makeDictionary (chunk:uniqueChunks) (key:keys) =
  (chunk, key) : makeDictionary uniqueChunks keys

checkArgs :: [String] -> Bool
checkArgs args
  | length args < 4 = False
  | otherwise = True
