module Pacrd.Stage where

import Data.Text (Text)
import Data.Yaml (FromJSON, Value, parseJSON, withObject, (.:))

data Stage = Stage {
    type'      :: !Text,
    properties :: !Value
    -- ^ Bag of properties. We can't get more specific here without re-implementing PaCRD.
  } deriving (Show)

instance FromJSON Stage where
  parseJSON = withObject "Stage" $ \o ->
    Stage <$> o .: "type" <*> o .: "properties"
