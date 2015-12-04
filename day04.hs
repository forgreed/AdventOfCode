{-# LANGUAGE OverloadedStrings #-}

import Data.Digest.Pure.MD5 -- cabal install pureMD5
import qualified Data.ByteString.Lazy.Char8 as C

input = "yzbqklnj"

hashIt :: String -> Int -> String
hashIt inp pad = show . md5 . C.pack $ inp ++ show pad

zeroHash :: String -> Int -> Int -> Int
zeroHash inp pad zeros
    | take zeros (hashIt inp pad) == replicate zeros '0' = pad
    | otherwise = zeroHash inp (pad+1) zeros

part1 = zeroHash input 0 5
part2 = zeroHash input 0 6
