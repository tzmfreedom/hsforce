{-# LANGUAGE OverloadedStrings #-}

module HSForce.Util
    ( defaultJsonLabelFilter,
      tagText,
      replace,
    ) where

import Data.List
import Data.Char
import Text.XML.HaXml
import Text.XML.HaXml.Util
import Text.XML.HaXml.Xtract.Parse

defaultJsonLabelFilter :: String -> String -> String
defaultJsonLabelFilter name = firstLower . drop (length name)

firstLower :: String -> String
firstLower (x:xs) = toLower x : xs
firstLower [] = error "Empty"

tagText :: Content a -> String -> String
tagText result xpath = do
  tagTextContent $ xtract id xpath result !! 0

replace :: String -> String -> String -> String
replace old new src = inner src where
  inner [] = []
  inner str@(x:xs)
    | isPrefixOf old str = new ++ inner (drop (length old) str)
    | otherwise = x:inner xs
