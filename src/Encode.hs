module Encode (encode) where
import Convert (stringToBits, splitByN)

encode :: String -> Int -> Int -> [[Int]]
encode str inputClass outputClass = splitByN (stringToBits str) inputClass
