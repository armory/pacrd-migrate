module Pacrd.Parameter where

import Data.Aeson   (ToJSON (toEncoding), defaultOptions, fieldLabelModifier,
                     genericToEncoding)
import Data.Text    (Text)
import Data.Yaml    (FromJSON)
import GHC.Generics (Generic)

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
instance ToJSON Parameter where
  toEncoding = genericToEncoding $ defaultOptions {
                 fieldLabelModifier = \case { "default'" -> "default"; s -> s }
               }

newtype Option = Option {
    value :: Text
  } deriving (Show, Generic)

instance FromJSON Option
instance ToJSON Option

