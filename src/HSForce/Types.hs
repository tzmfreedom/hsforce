{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module HSForce.Types where

import Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON, defaultOptions, Options(..))
import Data.Proxy as DP
import Data.Maybe
import Data.List as L
import GHC.Generics
import HSForce.Util

data QueryResponse a = QueryResponse{
  qrRecords :: [a],
  qrTotalSize :: Int,
  qrDone :: Bool
} deriving Show

class SObject a where
  typeName :: a -> String
  getSfid :: a -> String

data DescribeResponse a = DescribeResponse{
  drObjectDescribe :: ObjectDescribe,
  drRecentItems :: [a]
} deriving Show

data ObjectDescribe = ObjectDescribe {
  odActivateable :: Bool,
  odCreateable :: Bool,
  odCustom :: Bool,
  odCustomSetting :: Bool,
  odDeletable :: Bool,
  odDeprecatedAndHidden :: Bool,
  odFeedEnabled :: Bool,
  odHasSubtypes :: Bool,
  odIsSubtype :: Bool,
  odKeyPrefix :: Maybe String,
  odLabel :: String,
  odLabelPlural :: String,
  odLayoutable :: Bool,
  odMergeable :: Bool,
  odMruEnabled :: Bool,
  odName :: String,
  odQueryable :: Bool,
  odReplicateable :: Bool,
  odRetrieveable :: Bool,
  odSearchable :: Bool,
  odTriggerable :: Bool,
  odUndeletable :: Bool,
  odUpdateable :: Bool,
  odUrls :: Object
} deriving Show

data GlobalDescribeResponse = GlobalDescribeResponse{
  gdEncoding :: String,
  gdMaxBatchSize :: Int,
  gdSobjects :: [ObjectDescribe]
} deriving Show

data DescribeDetail = DescribeDetail{
  ddName:: String
} deriving Show

deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "qr" } ''QueryResponse
deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "dr" } ''DescribeResponse
deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "od" } ''ObjectDescribe
deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "dd" } ''DescribeDetail
deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "gd" } ''GlobalDescribeResponse
