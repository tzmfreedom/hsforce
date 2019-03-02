{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module HSForce
    ( login,
      typeName,
      getSfid,
      HSForce.query,
      HSForce.insert,
      HSForce.update,
      HSForce.upsert,
      HSForce.delete,
      SObject,
      SFClient(..),
    ) where

import Network.HTTP.Conduit
import Network.URI
import Network.URI.Encode as URI
import System.IO
import Data.Aeson as JSON
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

data SFClient = SFClient {
  accessToken :: String,
  instanceUrl :: String,
  apiVersion:: String,
  debug :: Bool
} deriving Show

data QueryResponse a = QueryResponse{
  records :: [a],
  totalSize :: Int,
  done :: Bool
} deriving Show

instance (FromJSON a) => FromJSON (QueryResponse a) where
  parseJSON = withObject "QueryResponse" $ \v -> do
    records <- v .: "records"
    totalSize <- v .: "totalSize"
    done <- v .: "done"
    return QueryResponse{..}

class SObject a where
  typeName :: a -> String
  getSfid :: a -> String

replace :: String -> String -> String -> String
replace old new src = inner src where
  inner [] = []
  inner str@(x:xs)
    | L.isPrefixOf old str = new ++ inner (L.drop (L.length old) str)
    | otherwise = x:inner xs

login :: String -> String -> String -> String -> IO (SFClient)
login username password endpoint apiVersion = do
  let body = "<?xml version=\"1.0\" encoding=\"utf-8\"?> \
  \<env:Envelope xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:env=\"http://schemas.xmlsoap.org/soap/envelope/\"> \
    \<env:Body> \
      \<n1:login xmlns:n1=\"urn:partner.soap.sforce.com\"> \
        \<n1:username>{username}</n1:username> \
       \<n1:password>{password}</n1:password> \
     \</n1:login> \
   \</env:Body> \
  \</env:Envelope>" :: String
  initReq <- parseRequest $ "https://" ++ endpoint ++ "/services/Soap/u/" ++ apiVersion
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
      accessToken = tagText result "/result/sessionId"
      serverUrl = tagText result "/result/serverUrl"
      matches = serverUrl =~ ("^(https://[^/]*)/.*" :: String) :: [[String]]
  return SFClient{accessToken, apiVersion, instanceUrl = matches !! 0 !! 1, debug = False}

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
  response <- requestDelete client path $ BL8.unpack $ JSON.encode object
  return ()

requestGet :: SFClient -> String -> IO (Response BL8.ByteString)
requestGet client path = do
  initReq <- parseRequest $ instanceUrl client ++ path
  manager <- newManager tlsManagerSettings
  let req = initReq {
    requestHeaders = [("Authorization", B8.pack $ "Bearer " ++ (accessToken client))]
  }
  if debug client then print req else pure ()
  response <- httpLbs req manager
  if debug client then print response else pure ()
  return (response)

requestWithBody :: B8.ByteString -> SFClient -> String -> String -> IO (Response BL8.ByteString)
requestWithBody method client path body = do
  initReq <- parseRequest $ instanceUrl client ++ path
  manager <- newManager tlsManagerSettings
  let req = initReq {
    method = method,
    requestHeaders = [
      ("Content-Type", "application/json"),
      ("Authorization", B8.pack $ "Bearer " ++ (accessToken client))
    ],
    requestBody = RequestBodyBS $ B8.pack body
  }
  if debug client then print req else pure ()
  response <- httpLbs req manager
  if debug client then print response else pure ()
  return (response)

requestPost :: SFClient -> String -> String -> IO (Response BL8.ByteString)
requestPost = requestWithBody "POST"

requestPatch :: SFClient -> String -> String -> IO (Response BL8.ByteString)
requestPatch = requestWithBody "PATCH"

requestDelete :: SFClient -> String -> String -> IO (Response BL8.ByteString)
requestDelete = requestWithBody "DELETE"

dataPath :: SFClient -> String
dataPath client = do
  "/services/data/" ++ (apiVersion client)

tagText :: Content a -> String -> String
tagText result xpath = do
  tagTextContent $ xtract id xpath result !! 0
