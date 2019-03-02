{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import HSForce

import System.Environment
import Data.Aeson as JSON
import Data.Proxy as DP
import Data.Maybe

data Account = Account{
  sfid :: Maybe String,
  name :: Maybe String,
  ex :: Maybe String
} deriving Show

instance SObject Account where
  typeName a = "Account"
  getSfid = fromJust . sfid

instance FromJSON Account where
  parseJSON = withObject "Account" $ \v -> do
    sfid <- v .: "Id"
    name <- v .:? "Name"
    ex <- v .:? "Ex__c"
    return Account{..}

instance ToJSON Account where
  toJSON (Account{sfid, name, ex}) =
    object ["Name" .= name]

main :: IO ()
main = do
  username <- getEnv "SALESFORCE_USERNAME"
  password <- getEnv "SALESFORCE_PASSWORD"
  endpoint <- getEnv "SALESFORCE_ENDPOINT"
  version <- getEnv "SALESFORCE_VERSION"
  client <- login username password endpoint version
  let client' = client { debug = True}
--  insert client Account{name="hogehoge"}
--  update client Account{sfid="0016F00003AfgQHQAZ", name="foobar"}
  upsert client' Account{sfid=Nothing, name = Just "foobar", ex = Just "aaa"} "Ex__c" "aaa"
  print =<< query client' "SELECT Id, Name FROM Account WHERE Name = 'foobar'" (Proxy :: Proxy Account)
