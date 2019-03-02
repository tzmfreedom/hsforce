{-# LANGUAGE OverloadedStrings #-} {-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module HSForce
    ( login,
      login',
      typeName,
      getSfid,
      HSForce.query,
      HSForce.insert,
      HSForce.update,
      HSForce.upsert,
      HSForce.delete,
      HSForce.describe,
      HSForce.describeDetail,
      HSForce.describeGlobal,
      SObject,
      SFClient(..),
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

data SFClient = SFClient {
  clientAccessToken :: String,
  clientInstanceUrl :: String,
  clientApiVersion:: String,
  clientDebug :: Bool
} deriving Show

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

replace :: String -> String -> String -> String
replace old new src = inner src where
  inner [] = []
  inner str@(x:xs)
    | L.isPrefixOf old str = new ++ inner (L.drop (L.length old) str)
    | otherwise = x:inner xs

login :: String -> String -> String -> String -> IO (SFClient)
login username password endpoint clientApiVersion = do
  let body = "<?xml version=\"1.0\" encoding=\"utf-8\"?> \
  \<env:Envelope xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:env=\"http://schemas.xmlsoap.org/soap/envelope/\"> \
    \<env:Body> \
      \<n1:login xmlns:n1=\"urn:partner.soap.sforce.com\"> \
        \<n1:username>{username}</n1:username> \
       \<n1:password>{password}</n1:password> \
     \</n1:login> \
   \</env:Body> \
  \</env:Envelope>" :: String
  initReq <- parseRequest $ "https://" ++ endpoint ++ "/services/Soap/u/" ++ clientApiVersion
  manager <- newManager tlsManagerSettings
  let requestBody = L.foldl (\body (bind,value) -> replace bind (escapeXML value) body) body [("{username}", username), ("{password}", password)]
  let req = initReq {
    method = "POST",
    requestHeaders = [("Content-Type", "text/xml"), ("SOAPAction", "''")],
    requestBody = RequestBodyBS $ B8.pack requestBody
  }
  response <- httpLbs req manager
  let Document _ _ root _ = xmlParse "" $ (BL8.unpack (responseBody response))
      cont = CElem root noPos
      result = xtract id "/soapenv:Envelope/soapenv:Body/loginResponse/result" cont !! 0
      clientAccessToken = tagText result "/result/sessionId"
      clientServerUrl = tagText result "/result/serverUrl"
      matches = clientServerUrl =~ ("^(https://[^/]*)/.*" :: String) :: [[String]]
  return SFClient{clientAccessToken, clientApiVersion, clientInstanceUrl = matches !! 0 !! 1, clientDebug = False}

login' :: IO (SFClient)
login' = do
  username <- getEnv "SALESFORCE_USERNAME"
  password <- getEnv "SALESFORCE_PASSWORD"
  endpoint <- getEnv "SALESFORCE_ENDPOINT"
  version <- getEnv "SALESFORCE_VERSION"
  login username password endpoint version

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

requestGet :: SFClient -> String -> IO (Response BL8.ByteString)
requestGet = requestWithoutBody "GET"

requestDelete :: SFClient -> String -> IO (Response BL8.ByteString)
requestDelete = requestWithoutBody "DELETE"

requestWithoutBody :: B8.ByteString -> SFClient -> String -> IO (Response BL8.ByteString)
requestWithoutBody method client path = do
  initReq <- parseRequest $ clientInstanceUrl client ++ path
  manager <- newManager tlsManagerSettings
  let req = initReq {
    method = method,
    requestHeaders = [("Authorization", B8.pack $ "Bearer " ++ (clientAccessToken client))]
  }
  printDebug client req
  response <- httpLbs req manager
  printDebug client response
  return (response)

requestWithBody :: B8.ByteString -> SFClient -> String -> String -> IO (Response BL8.ByteString)
requestWithBody method client path body = do
  initReq <- parseRequest $ clientInstanceUrl client ++ path
  manager <- newManager tlsManagerSettings
  let req = initReq {
    method = method,
    requestHeaders = [
      ("Content-Type", "application/json"),
      ("Authorization", B8.pack $ "Bearer " ++ (clientAccessToken client))
    ],
    requestBody = RequestBodyBS $ B8.pack body
  }
  printDebug client req
  response <- httpLbs req manager
  printDebug client response
  return (response)

requestPost :: SFClient -> String -> String -> IO (Response BL8.ByteString)
requestPost = requestWithBody "POST"

requestPatch :: SFClient -> String -> String -> IO (Response BL8.ByteString)
requestPatch = requestWithBody "PATCH"

printDebug :: (Show a) => SFClient -> a -> IO ()
printDebug client var = do
  if clientDebug client then print var else pure ()

dataPath :: SFClient -> String
dataPath client = do
  "/services/data/" ++ (clientApiVersion client)

tagText :: Content a -> String -> String
tagText result xpath = do
  tagTextContent $ xtract id xpath result !! 0

deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "client" } ''SFClient
deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "qr" } ''QueryResponse
deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "dr" } ''DescribeResponse
deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "od" } ''ObjectDescribe
deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "dd" } ''DescribeDetail
deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "gd" } ''GlobalDescribeResponse
