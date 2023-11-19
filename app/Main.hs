module Main where

import Convert (stringToInt, dictionaryToString)
import Encode (encode)

import System.Environment
import Data.Word
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (unpack)

checkArgs :: [String] -> Bool
checkArgs args
  | length args < 4 = False
  | otherwise = True

main :: IO ()
main = do
  {
  args <- getArgs;
    if not . checkArgs $ args then
      putStrLn "Insufficient arguments"
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
               else do
               let dictionary = dictionaryToString (snd encodedContents)
               BS.writeFile outputFile $ fst encodedContents;
               writeFile (outputFile ++ ".dict") dictionary
               putStrLn $
                 "[" ++ inputFile ++ "] \n" ++ fileContents ++ "\n" ++
                 "|\n" ++
                 "|\n" ++
                 "V\n" ++
                 "[" ++ outputFile ++ "] \n" ++ unpack (fst encodedContents) ++ "\n" ++
                 "[Dictionary] \n" ++ dictionary
        else putStrLn "Input class size should be greater than 0"
  }
