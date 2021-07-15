{-|
Module      : Dinghy
Description : Types and operations in the Dinghy domain.
Copyright   : (c) Armory.io 2021
License     : BSD3
Maintainer  : Fernando Freire
Stability   : experimental
-}

{-# LANGUAGE DuplicateRecordFields #-}

module Dinghy where

import Data.Aeson         (ToJSON (toEncoding), Value, defaultOptions,
                           fieldLabelModifier, genericToEncoding,
                           omitNothingFields)
import Data.Text          (Text)
import Dinghy.Application
import GHC.Generics       (Generic)

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
data Pipeline = Pipeline {
    pId                  :: !(Maybe Text),
    pType                :: !(Maybe Text),
    name                 :: !Text,
    pApplication         :: !Text,
    description          :: !(Maybe Text),
    executionEngine      :: !(Maybe Text),
    parallel             :: !(Maybe Bool),
    limitConcurrent      :: !(Maybe Bool),
    keepWaitingPipelines :: !(Maybe Bool),
    stages               :: ![Value],
    triggers             :: !(Maybe [Value]),
    parameters           :: !(Maybe [Value]),
    notifications        :: !(Maybe [Value]),
    expectedArtifacts    :: !(Maybe [Value]),
    locked               :: !(Maybe Locked),
    spelEvaluator        :: !(Maybe Text)
  } deriving (Show, Generic)

instance ToJSON Pipeline where
  toEncoding = genericToEncoding $ defaultOptions {
                 fieldLabelModifier = \case { "pId" -> "id"; "pType" -> "type"; "pApplication" -> "application"; s -> s },
                 omitNothingFields = True
               }

data Locked = Locked {
    ui            :: Bool,
    allowUnlockUi :: Bool
  } deriving (Show, Generic)

instance ToJSON Locked
