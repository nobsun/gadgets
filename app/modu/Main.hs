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
    ; let spec = "-s" `elem` args
    ; let as = delete "-s" args
    ; case as of
        [target]          -> do
            { initModule "Lib" target
            ; bool (return ())
                   (initTestModule "LibSpec" (target ++ "Spec"))
                   . (spec &&)
                   =<< doesDirectoryExist "test"
            }
        [template,target] -> do
            { initModule template target
            ; bool (return ())
                   (initTestModule (template ++ "Spec") (target ++ "Spec"))
                   . (spec &&)
                   =<< doesDirectoryExist "test"
            }
        _                 -> usage pr
    }

usage :: String -> IO ()
usage pr = hPutStrLn stderr $ unwords
    [ "Usage:"
    , pr
    , "[-s]"
    , "[<template-module>]"
    , "<target-module>"
    ]
