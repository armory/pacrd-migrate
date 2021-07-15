module Pacrd.Trigger where

import Data.Text (Text)
import Data.Yaml (FromJSON, Value, parseJSON, withObject, (.:))

data Trigger = Trigger {
    type'      :: !Text,
    properties :: !Value
    -- ^ Bag of properties. We can't get more specific here without re-implementing PaCRD.
  } deriving (Show)

instance FromJSON Trigger where
  parseJSON = withObject "Trigger" $ \o ->
    Trigger <$> o .: "type" <*> o .: "properties"
