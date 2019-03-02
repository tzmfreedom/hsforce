{-# LANGUAGE OverloadedStrings #-} {-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module HSForce
    ( login,
      login',
      HSForce.query,
      HSForce.insert,
      HSForce.update,
      HSForce.upsert,
      HSForce.delete,
      HSForce.describe,
      HSForce.describeDetail,
      HSForce.describeGlobal,
      HSForce.Types.SObject(..),
      HSForce.Client.SFClient(..),
    ) where

import Network.HTTP.Conduit
import Network.URI
import Network.URI.Encode as URI
import System.IO
import System.Environment
import Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON, defaultOptions, Options(..))
import Data.ByteString.Char8 as B8
import Data.ByteString.Lazy.Char8 as BL8
import Data.Proxy as DP
import Data.Maybe
import Data.List as L
import Text.HTML.TagSoup.Entity
import Text.XML.HaXml
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Util
import Text.XML.HaXml.Xtract.Parse
import Text.Regex.Posix
import GHC.Generics
import HSForce.Util
import HSForce.Client
import HSForce.Types

query :: (FromJSON a) => SFClient -> String -> DP.Proxy a -> IO (QueryResponse a)
query client q _ = do
  let path = dataPath client ++ "/query?q=" ++ URI.encode q
  response <- requestGet client path
  let res = (JSON.decode $ responseBody response) :: (FromJSON a) => Maybe (QueryResponse a)
  return (fromJust res)

insert :: (SObject a, ToJSON a) => SFClient -> a -> IO ()
insert client object = do
  let path = dataPath client ++ "/sobjects/" ++ typeName object
  response <- requestPost client path $ BL8.unpack $ JSON.encode object
  return ()

update :: (SObject a, ToJSON a) => SFClient -> a -> IO ()
update client object = do
  let path = dataPath client ++ "/sobjects/" ++ typeName object ++ '/':getSfid object
  response <- requestPatch client path $ BL8.unpack $ JSON.encode object
  return ()

upsert :: (SObject a, ToJSON a) => SFClient -> a -> String -> String -> IO ()
upsert client object upsertKey upsertKeyValue = do
  let path = dataPath client ++ "/sobjects/" ++ typeName object ++ '/':upsertKey ++ '/':upsertKeyValue
  response <- requestPatch client path $ BL8.unpack $ JSON.encode object
  return ()

delete :: (SObject a, ToJSON a) => SFClient -> a -> IO ()
delete client object = do
  let path = dataPath client ++ "/sobjects/" ++ typeName object ++ '/':getSfid object
  response <- requestDelete client path
  return ()

describe :: (FromJSON a) => SFClient -> String -> DP.Proxy a -> IO (DescribeResponse a)
describe client objectName _ = do
  let path = dataPath client ++ "/sobjects/" ++ objectName
  response <- requestGet client path
  let res = (JSON.decode $ responseBody response) :: (FromJSON a) => Maybe (DescribeResponse a)
  return (fromJust res)

describeDetail :: SFClient -> String -> IO DescribeDetail
describeDetail client objectName = do
  let path = dataPath client ++ "/sobjects/" ++ objectName ++ "/describe"
  response <- requestGet client path
  let res = (JSON.decode $ responseBody response) :: Maybe DescribeDetail
  return (fromJust res)

describeGlobal :: SFClient -> IO GlobalDescribeResponse
describeGlobal client = do
  let path = dataPath client ++ "/sobjects"
  response <- requestGet client path
  let res = (JSON.decode $ responseBody response) :: Maybe GlobalDescribeResponse
  return (fromJust res)
