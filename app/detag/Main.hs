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

import Data.Maybe
import Data.Char
import Data.List

main :: IO ()
main = interact (unlines . mapMaybe detag . lines)

detag :: String -> Maybe String
detag l = case dropWhile isSpace l of
    l' | "<a name=\"s" `isPrefixOf` l' -> Nothing
       | "<a name="    `isPrefixOf` l' -> case dropWhile ('>' /=) l' of
            _:l''                            -> Just $ takeWhile ('<' /=) l''
            _                                -> error "invalid input"
       | otherwise                     -> Nothing
