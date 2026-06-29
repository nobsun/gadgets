{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Main where

import Data.ByteString.Char8 qualified as B
import Data.List
import Data.List.Split
import System.Environment
import System.Random
import Text.Printf

main :: IO ()
main = do
    { prog <- getProgName
    ; args <- getArgs
    ; g    <- getStdGen
    ; case args of
        u:n:fs -> if
            | "z" `isPrefixOf` prog -> output fs $ genrnds g (read u) (read n) AllowZero
            | "n" `isPrefixOf` prog -> output fs $ genrnds g (read u) (read n) AllowNeg
            | otherwise             -> output fs $ genrnds g (read u) (read n) OnlyPos
        _     -> usage prog
    }

genrnds :: StdGen -> Int -> Int -> RndFlg -> [Int]
genrnds g u n = \ case
    OnlyPos   -> take n $ randomRs (1,u) g
    AllowZero -> take n $ randomRs (0,u) g
    AllowNeg  -> take n $ zipWith phi [1 ..] $ randomRs (0,u) g
        where phi i r = if odd (i+r) then negate r else r

output :: [String] -> [Int] -> IO ()
output flg = case flg of
    "-c":c:_ -> B.putStr . format (read c) . map (B.pack . show)
    _        -> B.putStrLn . B.unwords . map (B.pack . show)

format :: Int -> [B.ByteString] -> B.ByteString
format w = B.unlines . map B.unwords . chunksOf w

data RndFlg
    = AllowZero
    | AllowNeg
    | OnlyPos
    deriving (Eq, Show)

usage :: String -> IO ()
usage prog = printf "%s <upper bound> <number of numbers> [-c <number of columns>]\n" prog
