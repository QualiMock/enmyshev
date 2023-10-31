module Main where

import Encode (encode)
import Convert (stringToInt)

import System.Environment
import System.IO
import GHC.Read (list)
import Data.List (nub)

checkArgs :: [String] -> Bool
checkArgs args
  | length args < 4 = False
  | otherwise = True

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
             let encodedContents = encode fileContents inputClass outputClass
             if null encodedContents then
               putStrLn $ "File " ++ inputFile ++ " cannot be encoded\n\t"
               ++ "with output class size " ++ show outputClass
               else mapM_ putStrLn encodedContents
        else putStrLn "Input class size should be greater than 0"
      -- writeFile outputFile encryptedContents;
      -- print ("Encode " ++ inputFile ++ " -> " ++ outputFile)
  }
