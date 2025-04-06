{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Data.List
import Data.Time
import System.IO
import System.Directory

main :: IO ()
main = do
    { ss <- ("snapshot: nightly-" ++) . showGregorian . localDay <$> (utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime)
    ; ih <- openFile "stack.yaml" ReadMode
    ; oh <- openFile "tmptmp" WriteMode
    ; ls <- lines <$> hGetContents ih
    ; hPutStr oh $ unlines $ nightly ss ls (drop 1 ls); hFlush oh
    ; hClose oh
    ; hClose ih
    ; renameFile "stack.yaml" "stack.yaml.old"
    ; renameFile "tmptmp" "stack.yaml"
    ; removeFile "stack.yaml.old"
    }

nightly :: String -> [String] -> [String] -> [String]
nightly ss = \ case
    []         -> error "nightly: no contents"
    pps@(p:ps) -> \ case
        []         -> pps
        _:qs | "snapshot: " `isPrefixOf` p -> ss : "" : qs
             | otherwise                   -> p : nightly ss ps qs
