module Main where

import Data.Bool
import System.Environment
import System.IO
import System.Directory
import ModuleFile

main :: IO ()
main = do
    { pr <- getProgName
    ; as <- getArgs
    ; case as of
        [target]          -> do
            { initModule "Lib" target
            ; bool (return ())
                   (initTestModule "LibSpec" (target ++ "Spec"))
                   =<< doesDirectoryExist "test"
            }
        [template,target] -> do
            { initModule template target
            ; bool (return ())
                   (initTestModule (template ++ "Spec") (target ++ "Spec"))
                   =<< doesDirectoryExist "test"
            }
        _                 -> usage pr
    }

usage :: String -> IO ()
usage pr = hPutStrLn stderr $ unwords
    [ "Usage:"
    , pr
    , "[<template-module>]"
    , "<target-module>"
    ]

