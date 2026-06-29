{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Main where

import Data.ByteString.Char8 qualified as B
import Data.Bool
import Data.List
import Data.List.Split
import System.Environment
import System.IO
import System.Random
import Text.Printf

main :: IO ()
main = do
    { args <- getArgs
    ; g <- getStdGen
    ; case args of
        u:h:w:_ -> do
            { let ub = read u :: Int
            ; let (r,c) = (read h, read w) :: (Int, Int)
            ; let rs = take (r*c) $ randomRs (1,ub) g
            ; printf "%d %d\n" r c
            ; B.putStr $ B.unlines $ map (B.unwords . map (B.pack . show)) $ chunksOf c rs
            }
        _       -> usage
    }

usage :: IO ()
usage = hPutStrLn stderr "usage: rndints <upper bound> <number of rows> <number of columns>"
