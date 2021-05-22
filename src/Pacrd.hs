{-|
Module : Pacrd
Description : Types and operations in the PaCRD domain.
Copyright : (c) Armory.io 2021
License : BSD3
Maintainer : Fernando Freire
Stability : experimental
-}

{-# LANGUAGE DuplicateRecordFields #-}

module Pacrd where

import Data.Text    (Text)
import Data.Yaml    (FromJSON, Value (String), parseJSON, withObject, (.:),
                     (.:?))
import GHC.Generics (Generic)

-- | Represents a PaCRD Application manifest.
data Application = Application {
    apiVersion :: !Text,
    kind       :: !Kind,
    metadata   :: !Metadata,
    appSpec    :: !ApplicationSpec
  } deriving (Show, Generic)

instance FromJSON Application where
  parseJSON = withObject "Application" $ \o ->
    Application <$> o .: "apiVersion" <*> o .: "kind" <*> o .: "metadata" <*> o .: "spec"

data ApplicationSpec = ApplicationSpec {
    email          :: !(Maybe Text),
    appDescription :: !(Maybe Text),
    dataSources    :: !(Maybe DataSources),
    permissions    :: !(Maybe Permissions)
  } deriving (Show, Generic)

instance FromJSON ApplicationSpec where
  parseJSON = withObject "ApplicationSpec" $ \o ->
    ApplicationSpec <$> o .:? "email" <*> o.:? "description" <*> o.:? "dataSources" <*> o.:? "permissions"

data Permissions = Permissions {
    read    :: !(Maybe [Text]),
    write   :: !(Maybe [Text]),
    execute :: !(Maybe [Text])
  } deriving (Show)

instance FromJSON Permissions where
  parseJSON = withObject "Permissions" $ \v ->
    Permissions
    <$> v .:? "READ"
    <*> v .:? "WRITE"
    <*> v .:? "EXECUTE"

data DataSources = DataSources {
    enabled  :: !(Maybe [DataSource]),
    disabled :: !(Maybe [DataSource])
  } deriving (Show, Generic)

instance FromJSON DataSources

data DataSource =
  ServerGroup | Execution | LoadBalancer | SecurityGroup
  deriving (Show)

instance FromJSON DataSource where
  parseJSON (String s) =
    case s of
      "serverGroups"   -> pure ServerGroup
      "executions"     -> pure Execution
      "loadBalancer"   -> pure LoadBalancer
      "securityGroups" -> pure SecurityGroup
      _                -> fail "not a valid data source"
  parseJSON _ = fail "not a string"

data Kind = KindPipeline | KindApplication deriving (Show)

instance FromJSON Kind where
  parseJSON (String s) =
    case s of
      "Pipeline"    -> pure KindPipeline
      "Application" -> pure KindApplication
      _             -> fail "not a supported kind"
  parseJSON _ = fail "not a string"

-- | Represents a PaCRD Pipeline manifest.
data Pipeline = Pipeline {
  apiVersion :: !Text,
  metadata   :: !Metadata,
  kind       :: !Kind,
  spec       :: !PipelineSpec
} deriving (Show, Generic)

instance FromJSON Pipeline

newtype Metadata = Metadata {
    name :: Text
  } deriving (Show, Generic)

instance FromJSON Metadata

data PipelineSpec = PipelineSpec {
    application             :: !Text,
    description             :: !(Maybe Text),
    parameterConfig         :: !(Maybe [Parameter]),
    expectedArtifacts       :: !(Maybe [Artifact]),
    executionEngine         :: !(Maybe Text),
    allowParallelExecutions :: !(Maybe Bool),
    limitConcurrent         :: !(Maybe Bool),
    keepWaitingPipelines    :: !(Maybe Bool),
    stages                  :: ![Stage],
    triggers                :: !(Maybe [Trigger])
  } deriving (Show, Generic)

instance FromJSON PipelineSpec

data Stage = Stage {
    type'      :: !Text,
    properties :: !Value
    -- ^ Bag of properties. We can't get more specific here without re-implementing PaCRD.
  } deriving (Show)

instance FromJSON Stage where
  parseJSON = withObject "Stage" $ \o ->
    Stage <$> o .: "type" <*> o .: "properties"

data Trigger = Trigger {
    type'      :: !Text,
    properties :: !Value
    -- ^ Bag of properties. We can't get more specific here without re-implementing PaCRD.
  } deriving (Show)

instance FromJSON Trigger where
  parseJSON = withObject "Trigger" $ \o ->
    Trigger <$> o .: "type" <*> o .: "properties"

data Parameter = Parameter {
    default'    :: !Text,
    description :: !Text,
    hasOptions  :: !Bool,
    label       :: !Text,
    name        :: !Text,
    options     :: !Option,
    pinned      :: !Bool,
    required    :: !Bool
  } deriving (Show, Generic)

instance FromJSON Parameter

newtype Option = Option {
    value :: Text
  } deriving (Show, Generic)

instance FromJSON Option

data Artifact = Artifact {
    id                 :: !Text,
    displayName        :: !Text,
    usePriorArtifact   :: !(Maybe Bool),
    useDefaultArtifact :: !(Maybe Bool),
    defaultArtifact    :: !(Maybe MatchArtifact),
    matchArtifact      :: !(Maybe MatchArtifact)
  } deriving (Show, Generic)

instance FromJSON Artifact

data MatchArtifact = MatchArtifact {
    type'      :: !Text,
    properties :: !Value
    -- ^ Bag of properties. We can't get more specific here without re-implementing PaCRD.
  } deriving (Show, Generic)

instance FromJSON MatchArtifact where
  parseJSON = withObject "MatchArtifact" $ \o ->
    MatchArtifact
    <$> o .: "type"
    <*> o .: "properties"
