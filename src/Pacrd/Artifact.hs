{-# LANGUAGE NamedFieldPuns #-}

module Pacrd.Artifact where

import           Data.Aeson          (ToJSON (toEncoding, toJSON),
                                      defaultOptions, fieldLabelModifier,
                                      genericToEncoding, omitNothingFields)
import qualified Data.HashMap.Strict as HM
import           Data.Text           (Text)
import           Data.Yaml           (FromJSON, Value (Object, String),
                                      parseJSON, withObject, (.:))
import           GHC.Generics        (Generic)

data Artifact = Artifact {
    id                 :: !Text,
    displayName        :: !Text,
    usePriorArtifact   :: !(Maybe Bool),
    useDefaultArtifact :: !(Maybe Bool),
    defaultArtifact    :: !(Maybe MatchArtifact),
    matchArtifact      :: !(Maybe MatchArtifact)
  } deriving (Show, Generic)

instance FromJSON Artifact
instance ToJSON Artifact where
  toEncoding = genericToEncoding $ defaultOptions {
                 omitNothingFields = True
               }

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

instance ToJSON MatchArtifact where
  toJSON MatchArtifact { type', properties } =
    case properties of
        (Object p) -> Object $ HM.insert "type" (String type') p
        p'         -> p'
