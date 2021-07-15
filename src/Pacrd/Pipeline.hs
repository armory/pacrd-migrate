module Pacrd.Pipeline where

import Data.Text       (Text)
import Data.Yaml       (FromJSON, Value, parseJSON, withObject, (.:))
import GHC.Generics    (Generic)
import Pacrd.Artifact
import Pacrd.Internal
import Pacrd.Parameter
import Pacrd.Stage
import Pacrd.Trigger

-- | Represents a PaCRD Pipeline manifest.
data Pipeline = Pipeline {
  apiVersion :: !Text,
  metadata   :: !Metadata,
  kind       :: !Kind,
  spec       :: !PipelineSpec
} deriving (Show, Generic)

instance FromJSON Pipeline

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
