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

import Data.Bool
import Data.List
import List.Shuffle
import System.Environment

main :: IO ()
main = do
    { args <- getArgs
    ; let n = bool 1000 (read @Int (head args)) (not (null args))
    ; rs <- shuffleIO [1 .. n]
    ; print n
    ; putStr (unlines (singleton (unwords (show <$> rs))))
    ; }