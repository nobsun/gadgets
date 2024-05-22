module Main where

import System.Environment
import System.IO
import ModuleFile

main :: IO ()
main = do
    { pr <- getProgName
    ; as <- getArgs
    ; case as of
        [target]          -> initModule "Lib" target
        [template,target] -> initModule template target
        _                 -> usage pr
    }

usage :: String -> IO ()
usage pr = hPutStrLn stderr $ unwords
    [ "Usage:"
    , pr
    , "[<template-module>]"
    , "<target-module>"
    ]

