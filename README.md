# hsforce

## Usage


Login and create Salesforce client.
```haskell
main :: IO ()
main = do
  username <- getEnv "SALESFORCE_USERNAME"
  password <- getEnv "SALESFORCE_PASSWORD"
  endpoint <- getEnv "SALESFORCE_ENDPOINT"
  version <- getEnv "SALESFORCE_VERSION"
  client <- login username password endpoint version
```

create data type that is SObject class.
```haskell
import Data.Aeson as JSON
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
```

CRUD API
```haskell
-- insert object
insert client Account{sfid = Nothing, name = Just "hogehoge", ex = Nothing}

-- update object
update client Account{sfid = Just "xxxx", name = Just "foobar"}

-- upsert object
upsert client Account{sfid = Nothing, name = Just "foobar", ex = Just "aaa"} "Ex__c" "aaa"

-- delete object
delete client Account{sfid = Just "xxxx"}

-- query
query client "SELECT Id, Name FROM Account WHERE Name = 'foobar'" (Proxy :: Proxy Account)

-- queryMore
queryMore client "/services/data/v20.0/query/01gD0000002HU6KIAW-2000" (Proxy :: Proxy Account)

-- queryAll
queryAll client "SELECT Id, Name FROM Account WHERE Name = 'foobar'" (Proxy :: Proxy Account)

-- queryAllMore
queryAllMore client "/services/data/v20.0/queryMore/01gD0000002HU6KIAW-2000" (Proxy :: Proxy Account)

-- explain
explain client "SELECT Id FROM Account"

-- describe
describe client "Account" (Proxy :: Proxy Account)

-- describeDetail
describeDetail client "Account"

-- describeGlobal
describeGlobal client

-- recordCount
recordCount client ["Account", "Contact", "Opportunity"]

-- versions
versions client
```
