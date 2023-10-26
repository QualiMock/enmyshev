module Main where

import Encode (encode, addMissingZeroes)
import Convert (stringToInt, stringToBinary)

import Data.Char (ord)
import System.Environment
import System.IO
import GHC.Read (list)

main = do
  {
    args <- getArgs;
    if not . checkArgs $ args then
      print "Insufficient arguments"
    else do
      let inputFile = head args
      let outputFile = args !! 1
      let inputClass = stringToInt (args !! 2)
      let outputClass = stringToInt (last args)
      fileContents <- readFile inputFile
      let binaryContents = stringToBinary fileContents
      let trailingZeroes = inputClass - (length binaryContents `rem` inputClass)
      let encodedContents = encode binaryContents inputClass outputClass;
      print encodedContents
      -- writeFile outputFile encryptedContents;
      -- print ("Encode " ++ inputFile ++ " -> " ++ outputFile)
  }

checkArgs :: [String] -> Bool
checkArgs args
  | length args < 4 = False
  | otherwise = True
