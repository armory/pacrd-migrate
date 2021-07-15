{-# LANGUAGE DeriveAnyClass #-}

module Pacrd.Application where

import Data.Hashable  (Hashable)
import Data.Text      (Text)
import Data.Yaml      (FromJSON, Value (String), parseJSON, withObject, (.:),
                       (.:?))
import GHC.Generics   (Generic)

import Pacrd.Internal

-- | Represents a PaCRD Application manifest.
data Application = Application {
    apiVersion :: !Text,
    kind       :: !Kind,
    metadata   :: !Metadata,
    appSpec    :: !ApplicationSpec
  } deriving (Show, Hashable, Generic)

instance Eq Application where
  Application { metadata = aMeta } == Application { metadata = bMeta } =
    name aMeta == name bMeta

instance FromJSON Application where
  parseJSON = withObject "Application" $ \o ->
    Application <$> o .: "apiVersion" <*> o .: "kind" <*> o .: "metadata" <*> o .: "spec"

data ApplicationSpec = ApplicationSpec {
    email          :: !(Maybe Text),
    appDescription :: !(Maybe Text),
    dataSources    :: !(Maybe DataSources),
    permissions    :: !(Maybe Permissions)
  } deriving (Show, Hashable, Generic)

instance FromJSON ApplicationSpec where
  parseJSON = withObject "ApplicationSpec" $ \o ->
    ApplicationSpec <$> o .:? "email" <*> o.:? "description" <*> o.:? "dataSources" <*> o.:? "permissions"

data Permissions = Permissions {
    read    :: !(Maybe [Text]),
    write   :: !(Maybe [Text]),
    execute :: !(Maybe [Text])
  } deriving (Show, Hashable, Generic)

instance FromJSON Permissions where
  parseJSON = withObject "Permissions" $ \v ->
    Permissions
    <$> v .:? "READ"
    <*> v .:? "WRITE"
    <*> v .:? "EXECUTE"

data DataSources = DataSources {
    enabled  :: !(Maybe [DataSource]),
    disabled :: !(Maybe [DataSource])
  } deriving (Show, Hashable, Generic)

instance FromJSON DataSources

data DataSource =
  ServerGroup | Execution | LoadBalancer | SecurityGroup
  deriving (Show, Hashable, Generic)

instance FromJSON DataSource where
  parseJSON (String s) =
    case s of
      "serverGroups"   -> pure ServerGroup
      "executions"     -> pure Execution
      "loadBalancer"   -> pure LoadBalancer
      "securityGroups" -> pure SecurityGroup
      _                -> fail "not a valid data source"
  parseJSON _ = fail "not a string"
