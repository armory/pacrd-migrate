{-|
Module      : Migrate
Description : Operations in the pure migrations domain.
Copyright   : (c) Armory.io 2021
License     : BSD3
Maintainer  : Fernando Freire
Stability   : experimental
-}

module Migrate where

import           Data.Yaml           (Value (Object, String), toJSON)

import qualified Data.HashMap.Strict as HM
import qualified Dinghy
import qualified Dinghy.Application  as DinApp
import qualified Pacrd
import qualified Pacrd.Application   as PacApp
import qualified Pacrd.Internal      as Internal
import qualified Pacrd.Pipeline      as PacPipe
import qualified Pacrd.Stage         as PacStage
import qualified Pacrd.Trigger       as PacTrigger

convertPermissions :: PacApp.Permissions -> DinApp.Permissions
convertPermissions (PacApp.Permissions read write execute) =
    DinApp.Permissions read write execute

convertDataSource :: PacApp.DataSource -> DinApp.DataSource
convertDataSource PacApp.ServerGroup   = DinApp.ServerGroup
convertDataSource PacApp.LoadBalancer  = DinApp.LoadBalancer
convertDataSource PacApp.Execution     = DinApp.Execution
convertDataSource PacApp.SecurityGroup = DinApp.SecurityGroup

convertDataSources :: PacApp.DataSources -> DinApp.DataSources
convertDataSources (PacApp.DataSources enabled disabled) = DinApp.DataSources
    ((fmap . fmap $ convertDataSource) enabled)
    ((fmap . fmap $ convertDataSource) disabled)

convertApp :: PacApp.Application -> DinApp.Application
convertApp pacrdApp = DinApp.Application
    { DinApp.email       = PacApp.email . PacApp.appSpec $ pacrdApp
    , DinApp.description = PacApp.appDescription . PacApp.appSpec $ pacrdApp
    , DinApp.permissions = convertPermissions
        <$> (PacApp.permissions . PacApp.appSpec $ pacrdApp)
    , DinApp.dataSources = convertDataSources
        <$> (PacApp.dataSources . PacApp.appSpec $ pacrdApp)
    }

convertPacrdStage :: PacStage.Stage -> Value
convertPacrdStage PacStage.Stage { PacStage.type' = t, PacStage.properties = p }
    = case p of
        (Object p') -> Object $ HM.insert "type" (String t) p'
        p'          -> p'

-- TODO implement as ToJSON
convertPacrdTrigger :: PacTrigger.Trigger -> Value
convertPacrdTrigger PacTrigger.Trigger { PacTrigger.type' = t, PacTrigger.properties = p }
    = case p of
        (Object p') -> Object $ HM.insert "type" (String t) p'
        p'          -> p'

convertPipeline :: PacPipe.Pipeline -> Dinghy.Pipeline
convertPipeline pacrdPipeline
    = let spec = PacPipe.spec pacrdPipeline
      in
          Dinghy.Pipeline
              { Dinghy.pId                  = Nothing
              , Dinghy.pType                = Nothing
              , Dinghy.name = Internal.name . PacPipe.metadata $ pacrdPipeline
              , Dinghy.pApplication         = PacPipe.application
                  . PacPipe.spec
                  $ pacrdPipeline
              , Dinghy.description          = PacPipe.description
                  . PacPipe.spec
                  $ pacrdPipeline
              , Dinghy.executionEngine      = PacPipe.executionEngine spec
              , Dinghy.parallel             = Nothing
              , Dinghy.limitConcurrent      = PacPipe.limitConcurrent spec
              , Dinghy.keepWaitingPipelines = PacPipe.keepWaitingPipelines spec
              , Dinghy.stages = convertPacrdStage <$> PacPipe.stages spec
              , Dinghy.triggers             = (fmap . fmap) convertPacrdTrigger
                  $ PacPipe.triggers spec
              , Dinghy.parameters           = (fmap . fmap) toJSON
                  $ PacPipe.parameterConfig spec
              , Dinghy.notifications        = Nothing
              , Dinghy.expectedArtifacts    = (fmap . fmap) toJSON
                  $ PacPipe.expectedArtifacts spec
              , Dinghy.locked               = Nothing
              , Dinghy.spelEvaluator        = Nothing
              }

toDinghyfile :: PacApp.Application -> [PacPipe.Pipeline] -> Dinghy.Dinghyfile
toDinghyfile app ps = Dinghy.Dinghyfile
    { Dinghy.application = Internal.name . PacApp.metadata $ app
    , Dinghy.spec        = Nothing
    , Dinghy.pipelines   = convertPipeline <$> ps
    }

convertPacrdTree :: Pacrd.PacrdTree -> [Dinghy.Dinghyfile]
convertPacrdTree t = fmap (uncurry toDinghyfile) $ snd <$> HM.toList t
