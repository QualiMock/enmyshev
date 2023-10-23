module Main where

import Encode (encode)

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
      let inputFile = head args;
      let outputFile = args !! 1;
      let inputClass = args !! 2;
      let outputClass = last args;
      fileContents <- readFile inputFile;
      let encryptedContents = encode fileContents;
      writeFile outputFile encryptedContents;
      print ("Encode " ++ inputFile ++ " -> " ++ outputFile)
  }

checkArgs :: [String] -> Bool
checkArgs args
  | length args < 4 = False
  | otherwise = True
