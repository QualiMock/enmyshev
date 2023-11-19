module Encode (encode) where

import Convert (toBinary, joinList, splitByNBits, toStringOfBinaries, fromBinary)

import qualified Codec.Binary.UTF8.String as UTF
import Data.ByteString (pack, unpack, ByteString)
import Data.ByteString.UTF8 (fromString)
import Data.Word (Word8)
import Data.List
import qualified System.Random as Rnd (randoms, mkStdGen)
import qualified Data.ByteString as ByteString

getMinOutputClass uniqueChunks = ceiling $ logBase 2 $ fromIntegral . length $ uniqueChunks

makeDictionary [] [] = []
makeDictionary [] _ = []
makeDictionary _ [] = []
makeDictionary (chunk:uniqueChunks) (key:keys) =
  (chunk, key) : makeDictionary uniqueChunks keys

substitute (pair:dictionary) str
  | str == fst pair = snd pair
  | otherwise = substitute dictionary str

encode :: String -> Int -> Int -> (ByteString, [([Bool], [Bool])])
encode str inputClass outputClass = do
    let binaryString = joinList $ map toBinary $ unpack $ fromString str
    let splet = splitByNBits binaryString inputClass
    let chunks = fst splet
    let trailingZeroes = snd splet
    let uniqueChunks = nub chunks
    if outputClass >= getMinOutputClass uniqueChunks
      then do
      let encodeSequence = take (length chunks) $ nub $
                           map (drop (8 - outputClass) . toBinary) (Rnd.randoms (Rnd.mkStdGen 42) :: [Word8])
      let dictionary = makeDictionary uniqueChunks encodeSequence
      let encoded = joinList $ map (substitute dictionary) chunks
      let addedZeroes = length encoded `mod` 8
      let output = encoded ++ replicate addedZeroes False
      (pack $ map fromBinary $ fst (splitByNBits output 8), dictionary)
      else
      (ByteString.empty, [])
