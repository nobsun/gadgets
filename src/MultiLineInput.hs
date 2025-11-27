-- # MultiLineInput
-- 
-- ## 言語拡張と`module`宣言
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module MultiLineInput
    ( multiLineInput
    ) where

import Data.Bool
import Data.Char
import Data.List
import System.Console.Haskeline
import System.IO.Unsafe

multiLineInput :: String -> String -> Char -> IO [String]
multiLineInput p q t = loop id where
    loop :: (String -> String) -> IO [String]
    loop acc = unsafeInterleaveIO $ do
        { minput <- runInputT defaultSettings (getInputLine p)
            ; case minput of
            Nothing -> return []
            Just input
                | input == q -> return []
                | otherwise  -> case break (t ==) input' of
                    (_,[])       -> loop (acc' . (input' ++))
                    (str,[_])    -> (acc' str :) <$> loop id
                    _            -> error "invalid input"
                where
                    input' = trim input
                    acc'   = bool (acc . (' ' :)) id (acc "" == "")
        }

-- multiLineInput q t = runInputT defaultSettings (loop id) where
--     loop :: (String -> String) -> InputT IO [String]
--     loop acc = do
--         { minput <- getInputLine ""
--         ; case minput of
--             Nothing -> return []
--             Just input
--                 | input == q -> return []
--                 | otherwise  -> case break (t ==) input' of
--                     (_,[])       -> loop (acc' . (input' ++))
--                     (str,[_])    -> (acc' str :) <$> loop id
--                     _            -> error "invalid input"
--                 where
--                     input' = trim input
--                     acc'   = bool (acc . (' ' :)) id (acc "" == "")
--         }

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
