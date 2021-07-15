{-# LANGUAGE DeriveAnyClass #-}

module Pacrd.Internal where

import Data.Hashable (Hashable)
import Data.Text     (Text)
import Data.Yaml     (FromJSON, Value (String), parseJSON)
import GHC.Generics  (Generic)

data Kind = KindPipeline | KindApplication deriving (Show, Hashable, Generic)

instance FromJSON Kind where
  parseJSON (String s) =
    case s of
      "Pipeline"    -> pure KindPipeline
      "Application" -> pure KindApplication
      _             -> fail "not a supported kind"
  parseJSON _ = fail "not a string"

newtype Metadata = Metadata {
    name :: Text
  } deriving (Show, Hashable, Generic)

instance FromJSON Metadata
