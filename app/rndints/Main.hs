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

import Data.Bool
import Data.List
import System.Environment
import System.Random

main :: IO ()
main = do
    { args <- getArgs
    ; g <- getStdGen
    ; case args of
        n:u:f -> do
            { let num = read @Int n
            ; let ub = read @Int u
            ; let rs = take num $ randomRs (1,ub) g
            ; print num
            ; case f of
                "-m":_ -> putStr $ unlines $ map show rs
                _      -> putStr $ unlines $ singleton $ unwords $ map show rs
            }
        _          -> usage
    }

usage :: IO ()
usage = putStrLn "usage: rndints <number of numbers> <upper bound> [-m]"