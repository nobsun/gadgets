{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Main where

import Control.Monad
import Data.Bool
import Data.List
import Data.List.Split
import System.Directory
import System.Environment
import System.FilePath
import System.IO

main :: IO ()
main = do
    { pn <- getProgName
    ; as <- getArgs
    ; case as of
        [a] -> do
            { exi <- doesFileExist i
            ; if not exi then hPutStrLn stderr $ "file not exists: " ++ i 
              else do
                { exo <- doesFileExist o 
                ; when exo (renameFile o s)
                ; writeFile o . proc =<< readFile i
                }
            }
            where
                i = tof a <.> "hspp"
                o = tof a <.> "hs"
                s = o <.> "orig"
        _   -> usage pn 
    }

usage :: String -> IO ()
usage pn = putStrLn ("usage: " ++ cmdline)
    where
        cmdline = unwords [pn, "<module name>" ]

tof :: String -> FilePath
tof = ("src" </>) . foldr1 (</>) . splitOn "."

proc :: String -> String
proc = unlines . map head . group . filter (not . ("#" `isPrefixOf`)) . lines
