{-|
Module : Dinghy
Description : Types and operations in the Dinghy domain.
Copyright : (c) Armory.io 2021
License : BSD3
Maintainer : Fernando Freire
Stability : experimental
-}

module Dinghy where

import Data.Aeson   (ToJSON (toEncoding), Value (String), defaultOptions,
                     genericToEncoding, object, omitNothingFields, toJSON, (.=))
import Data.Text    (Text)
import GHC.Generics (Generic)

-- | Subset of the Dinghyfile spec.
--
-- We avoid defining the entire spec to keep the implementation simple. We're
-- not going to spend enough time on this codebase to warrant fancier migrations
-- that would involve things like computing globals :shrug:.
data Dinghyfile = Dinghyfile {
    application :: !Text,
    spec        :: !(Maybe Application),
    pipelines   :: ![Pipeline]
  } deriving (Show, Generic)

instance ToJSON Dinghyfile where
  toEncoding = genericToEncoding $ defaultOptions { omitNothingFields = True }

-- | Subset of a Pipeline representation in Dinghy.
--
-- TODO
data Pipeline = Pipeline deriving (Show, Generic)

instance ToJSON Pipeline

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

