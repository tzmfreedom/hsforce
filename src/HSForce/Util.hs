{-# LANGUAGE OverloadedStrings #-}

module HSForce.Util
    ( defaultJsonLabelFilter,
    ) where

import Data.List
import Data.Char

defaultJsonLabelFilter :: String -> String -> String
defaultJsonLabelFilter name = firstLower . drop (length name)

firstLower :: String -> String
firstLower (x:xs) = toLower x : xs
firstLower [] = error "Empty"
