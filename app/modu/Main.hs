module Main where

import Data.List
import Data.Bool
import System.Environment
import System.IO
import System.Directory
import ModuleFile

main :: IO ()
main = do
    { pr <- getProgName
    ; args <- getArgs
    ; let test = any (`elem` args) ["-t","-b"]
    ; let src  = any (`elem` args) ["-s","-b"] || not test
    ; let as = args \\ ["-s","-t","-b"]
    ; case as of
        [target]          -> do
            { bool (return ()) (initModule "Lib" target) src
            ; bool (return ())
                   (initTestModule "LibSpec" (target ++ "Spec"))
                   . (test &&)
                   =<< doesDirectoryExist "test"
            }
        [template,target] -> do
            { bool (return ()) (initModule template target) src
            ; bool (return ())
                   (initTestModule (template ++ "Spec") (target ++ "Spec"))
                   . (test &&)
                   =<< doesDirectoryExist "test"
            }
        _                 -> usage pr
    }

usage :: String -> IO ()
usage pr = hPutStrLn stderr $ unwords
    [ "Usage:"
    , pr
    , "[-s] [-t] [-b]"
    , "[<template-module>]"
    , "<target-module>"
    ]
