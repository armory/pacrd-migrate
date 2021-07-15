{-|
Module      : Dinghy.Application
Description : Types and operations in the Dinghy domain.
Copyright   : (c) Armory.io 2021
License     : BSD3
Maintainer  : Fernando Freire
Stability   : experimental
-}

module Dinghy.Application where

import Data.Aeson   (ToJSON (toEncoding), Value (String), defaultOptions,
                     genericToEncoding, object, omitNothingFields, toJSON, (.=))
import Data.Text    (Text)
import GHC.Generics (Generic)

-- | Subset of an Application representation in Dinghy.
data Application = Application {
    email       :: !(Maybe Text),
    description :: !(Maybe Text),
    dataSources :: !(Maybe DataSources),
    permissions :: !(Maybe Permissions)
  } deriving (Show, Generic)

instance ToJSON Application where
  toEncoding = genericToEncoding $ defaultOptions { omitNothingFields = True }

data DataSources = DataSources {
    enabled  :: !(Maybe [DataSource]),
    disabled :: !(Maybe [DataSource])
  } deriving (Show, Generic)

instance ToJSON DataSources where
  toEncoding = genericToEncoding $ defaultOptions { omitNothingFields = True }

data DataSource =
  ServerGroup | Execution | LoadBalancer | SecurityGroup
  deriving (Show)

instance ToJSON DataSource where
  toJSON ServerGroup   = String "serverGroups"
  toJSON LoadBalancer  = String "executions"
  toJSON Execution     = String "loadBalancer"
  toJSON SecurityGroup = String "securityGroups"

data Permissions = Permissions {
    read    :: !(Maybe [Text]),
    write   :: !(Maybe [Text]),
    execute :: !(Maybe [Text])
  } deriving (Show)

instance ToJSON Permissions where
  toJSON (Permissions read write execute) = object [
      "READ"    .= toJSON read,
      "WRITE"   .= toJSON write,
      "EXECUTE" .= toJSON execute
    ]

