{-# LANGUAGE OverloadedStrings #-} {-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module HSForce.Client
    (
      defaultLoginRequest,
      login,
      restLogin,
      soapLogin,
      requestGet,
      requestPost,
      requestPatch,
      requestDelete,
      dataPath,
      SFClient(..),
      LoginRequest(..),
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
import Data.Text as T
import Data.HashMap.Strict as M
import Data.List as L
import Text.HTML.TagSoup.Entity
import Text.XML.HaXml
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Util
import Text.XML.HaXml.Xtract.Parse
import Text.Regex.Posix
import GHC.Generics
import HSForce.Util
import Control.Applicative

data SFClient = SFClient {
  clientAccessToken :: String,
  clientInstanceUrl :: String,
  clientApiVersion:: String,
  clientDebug :: Bool
} deriving Show

data LoginRequest = LoginRequest{
  sfUsername :: Maybe String,
  sfPassword :: Maybe String,
  sfEndpoint :: Maybe String,
  sfVersion :: Maybe String,
  sfClientID :: Maybe String,
  sfClientSecret :: Maybe String
} deriving Show

defaultLoginRequest :: IO LoginRequest
defaultLoginRequest = do
  sfUsername <- lookupEnv "SALESFORCE_USERNAME"
  sfPassword <- lookupEnv "SALESFORCE_PASSWORD"
  endpoint <- lookupEnv "SALESFORCE_ENDPOINT"
  version <- lookupEnv "SALESFORCE_VERSION"
  sfClientID <- lookupEnv "SALESFORCE_CLIENT_ID"
  sfClientSecret <- lookupEnv "SALESFORCE_CLIENT_SECRET"
  return (LoginRequest{
    sfUsername,
    sfPassword,
    sfEndpoint = endpoint <|> Just "login.salesforce.com",
    sfVersion = version <|> Just "v44.0",
    sfClientID,
    sfClientSecret
  })

login :: LoginRequest -> IO (SFClient)
login lr = do
  if isOAuth lr then restLogin lr else soapLogin lr

soapLogin :: LoginRequest -> IO (SFClient)
soapLogin LoginRequest{sfUsername, sfPassword, sfEndpoint, sfVersion} = do
  let username = fromJust sfUsername
      password = fromJust sfPassword
      endpoint = fromJust sfEndpoint
      clientApiVersion = fromJust sfVersion
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
  let requestBody = L.foldl (\body (bind,value) -> HSForce.Util.replace bind (escapeXML value) body) body [("{username}", username), ("{password}", password)]
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

restLogin :: LoginRequest -> IO (SFClient)
restLogin LoginRequest{sfUsername, sfPassword, sfEndpoint, sfVersion, sfClientID, sfClientSecret} = do
  let username = fromJust sfUsername
      password = fromJust sfPassword
      endpoint = fromJust sfEndpoint
      clientApiVersion = fromJust sfVersion
      clientId = fromJust sfClientID
      clientSecret = fromJust sfClientSecret
  initReq <- parseRequest $ "https://" ++ endpoint ++ "/services/oauth2/token"
  manager <- newManager tlsManagerSettings
  let params = [
        ("grant_type", "password"),
        ("client_id", clientId),
        ("client_secret", clientSecret),
        ("username", username),
        ("password", password)
        ]
      requestBody = L.tail $ L.foldl (\body (k,v) -> body ++ "&" ++ k ++ "=" ++ URI.encode v) "" params
      req = initReq {
        method = "POST",
        requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")],
        requestBody = RequestBodyBS $ B8.pack requestBody
      }
  response <- httpLbs req manager
  let tokenObject = fromJust (JSON.decode $ responseBody response :: Maybe Object)
      clientAccessToken = T.unpack . getText . fromJust $ M.lookup "access_token" tokenObject
      clientInstanceUrl = T.unpack . getText . fromJust $ M.lookup "instance_url" tokenObject
  return SFClient{clientAccessToken, clientApiVersion, clientInstanceUrl, clientDebug = False}

isOAuth :: LoginRequest -> Bool
isOAuth LoginRequest{sfClientID = Just _, sfClientSecret = Just _} = True
isOAuth _ = False

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

getText :: Value -> Text
getText (String a) = a

deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "client" } ''SFClient
