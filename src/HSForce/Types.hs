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
  ddActionOverrides :: Maybe [ActionOverride],
  ddActivateable :: Maybe Bool,
  ddChildRelationships :: Maybe [ChildRelationship],
  ddCompactLayoutable :: Maybe Bool,
  ddCreateable :: Maybe Bool,
  ddCustom :: Maybe Bool,
  ddCustomSetting :: Maybe Bool,
  ddDeletable :: Maybe Bool,
  ddDeprecatedAndHidden :: Maybe Bool,
  ddFeedEnabled :: Maybe Bool,
  ddFields :: Maybe [Field],
  ddHasSubtypes :: Maybe Bool,
  ddIsSubtype :: Maybe Bool,
  ddKeyPrefix :: Maybe String,
  ddLabel :: Maybe String,
  ddLabelPlural :: Maybe String,
  ddLayoutable :: Maybe Bool,
  ddListviewable :: Maybe Bool,
  ddLookupLayoutable :: Maybe Bool,
  ddMergeable :: Maybe Bool,
  ddMruEnabled :: Maybe Bool,
  ddName :: Maybe String,
  ddNamedLayoutInfos :: Maybe [String],
  ddNetworkScopeFieldName :: Maybe String,
  ddQueryable :: Maybe Bool,
  ddRecordTypeInfos :: Maybe [RecordTypeInfo],
  ddReplicateable :: Maybe Bool,
  ddRetrieveable :: Maybe Bool,
  ddSearchLayoutable :: Maybe Bool,
  ddSearchable :: Maybe Bool,
  ddSupportedScopes :: Maybe [Scope],
  ddTriggerable :: Maybe Bool,
  ddUndeletable :: Maybe Bool,
  ddUpdateable :: Maybe Bool,
  ddUrls :: Maybe Object
} deriving Show

data ActionOverride = ActionOverride{
  aoFormFactor :: Maybe String,
  aoIsAvailableInTouch :: Maybe Bool,
  aoName :: Maybe String,
  aoPageId :: Maybe String,
  aoUrl :: Maybe String
} deriving Show

data ChildRelationship = ChildRelationship{
  crCascadeDelete :: Maybe Bool,
  crChildSObject :: Maybe String,
  crDeprecatedAndHidden :: Maybe Bool,
  crField :: Maybe String,
  crJunctionIdListNames :: Maybe [String],
  crJunctionReferenceTo :: Maybe [String],
  crRelationshipName :: Maybe String,
  crRestrictedDelte :: Maybe Bool
} deriving Show

data Field = Field{
  fAggregatable :: Maybe Bool,
  fAiPredictionField :: Maybe Bool,
  fAutoNumber :: Maybe Bool,
  fByteLength :: Maybe Int,
  fCalculated :: Maybe Bool,
  fCalculatedFormula :: Maybe String,
  fCascadeDelete :: Maybe Bool,
  fCaseSensitive :: Maybe Bool,
  fCompoundFieldName :: Maybe String,
  fControllerName :: Maybe String,
  fCreateable :: Maybe Bool,
  fCustom :: Maybe Bool,
  fDefaultValue :: Maybe Bool,
  fDefaultValueFormula :: Maybe String,
  fDefaultedOnCreate :: Maybe Bool,
  fDependentPicklist :: Maybe Bool,
  fDeprecatedAndHidden :: Maybe Bool,
  fDigits :: Maybe Int,
  fDisplayLocationInDecimal :: Maybe Bool,
  fEncrypted :: Maybe Bool,
  fExternalId :: Maybe Bool,
  fExtraTypeInfo :: Maybe String,
  fFilterable :: Maybe Bool,
  fFilteredLookupInfo :: Maybe String,
  fFormulaTreatNullNumberAsZero :: Maybe Bool,
  fGroupable :: Maybe Bool,
  fHighScaleNumber :: Maybe Bool,
  fHtmlFormatted :: Maybe Bool,
  fIdLookup :: Maybe Bool,
  fInlineHelpText :: Maybe String,
  fLabel :: Maybe String,
  fLength :: Maybe Int,
  fMask :: Maybe String,
  fMaskType :: Maybe String,
  fName :: Maybe String,
  fNameField :: Maybe Bool,
  fNamePointing :: Maybe Bool,
  fNillable :: Maybe Bool,
  fPermissionable :: Maybe Bool,
  fPicklistValues :: Maybe [PicklistEntry],
  fPolymorphicForeignKey :: Maybe Bool,
  fPrecision :: Maybe Int,
  fQueryByDistance :: Maybe Bool,
  fReferenceTargetField :: Maybe String,
  fReferenceTo :: Maybe [String],
  fRelationshipName :: Maybe String,
  fRelationshipOrder :: Maybe Int,
  fRestrictedDelete :: Maybe Bool,
  fRestrictedPicklist :: Maybe Bool,
  fScale :: Maybe Int,
  fSearchPrefilterable :: Maybe Bool,
  fSoapType :: Maybe String,
  fSortable :: Maybe Bool,
  fType :: Maybe String,
  fUnique :: Maybe Bool,
  fUpdateable :: Maybe Bool,
  fWriteRequiresMasterRead :: Maybe Bool
} deriving Show

data PicklistEntry = PicklistEntry{
  peValidFor :: Maybe String,
  peActive :: Maybe Bool,
  peDefaultValue :: Maybe Bool,
  peLabel :: Maybe String,
  peValue :: Maybe String
} deriving Show

data RecordTypeInfo = RecordTypeInfo{
  rtAvailable :: Maybe Bool,
  rtDefaultRecordTypeMapping :: Maybe Bool,
  rtDeveloperName :: Maybe String,
  rtMaster :: Maybe Bool,
  rtName :: Maybe String,
  rtRecordTypeId :: Maybe String
} deriving Show

data Scope = Scope{
  scopeName :: Maybe String,
  scopeValue :: Maybe String
} deriving Show

data RecordCount = RecordCount{
  rcsObjects :: [SObjectCount]
} deriving Show

data SObjectCount = SObjectCount{
  scCount :: Int,
  scName :: String
} deriving Show

data Version = Version{
  vVersion :: String,
  vLabel :: String,
  vUrl :: String
} deriving Show

data BatchRequest a = BatchRequest{
  brBinaryPartName :: String,
  brMethod :: String,
  brRichInput :: a,
  brUrl :: String,
  brBinaryPartNameAlias :: String
} deriving Show

data Explain = Explain{
  ePlans :: [Plan],
  actionOverrides :: [String],
  activateable :: Bool
} deriving Show

data Plan = Plan{
  pCardinality :: Int,
  pFields :: [String],
  pNotes :: [Note],
  pRelativeCost :: Float,
  pSobjectCardinality :: Int,
  pSobjectType :: String
} deriving Show

data Note = Note{
  nDescription :: String,
  nFields :: [String],
  nTableEnumOrId :: String
} deriving Show

deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "qr" } ''QueryResponse
deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "dr" } ''DescribeResponse
deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "od" } ''ObjectDescribe
deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "dd" } ''DescribeDetail
deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "gd" } ''GlobalDescribeResponse
deriveJSON defaultOptions { fieldLabelModifier = drop 2 } ''RecordCount
deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "sc" } ''SObjectCount
deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "v" } ''Version
deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "br" } ''BatchRequest
deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "e" } ''Explain
deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "p" } ''Plan
deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "n" } ''Note
deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "f" } ''Field
deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "pe" } ''PicklistEntry
deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "cr" } ''ChildRelationship
deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "rt" } ''RecordTypeInfo
deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "scope" } ''Scope
deriveJSON defaultOptions { fieldLabelModifier = defaultJsonLabelFilter "ao" } ''ActionOverride
