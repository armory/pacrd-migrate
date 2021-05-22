{-|
Module : Migrate
Description : Operations in the pure migrations domain.
Copyright : (c) Armory.io 2021
License : BSD3
Maintainer : Fernando Freire
Stability : experimental
-}

module Migrate where

import qualified Dinghy
import qualified Pacrd

convertPermissions :: Pacrd.Permissions -> Dinghy.Permissions
convertPermissions (Pacrd.Permissions read write execute) =
    Dinghy.Permissions read write execute

convertDataSource :: Pacrd.DataSource -> Dinghy.DataSource
convertDataSource Pacrd.ServerGroup   = Dinghy.ServerGroup
convertDataSource Pacrd.LoadBalancer  = Dinghy.LoadBalancer
convertDataSource Pacrd.Execution     = Dinghy.Execution
convertDataSource Pacrd.SecurityGroup = Dinghy.SecurityGroup

convertDataSources :: Pacrd.DataSources -> Dinghy.DataSources
convertDataSources (Pacrd.DataSources enabled disabled) = Dinghy.DataSources
    ((fmap . fmap $ convertDataSource) enabled)
    ((fmap . fmap $ convertDataSource) disabled)

convertApp :: Pacrd.Application -> Dinghy.Application
convertApp pacrdApp = Dinghy.Application
    { Dinghy.email       = Pacrd.email . Pacrd.appSpec $ pacrdApp
    , Dinghy.description = Pacrd.appDescription . Pacrd.appSpec $ pacrdApp
    , Dinghy.permissions = convertPermissions
        <$> (Pacrd.permissions . Pacrd.appSpec $ pacrdApp)
    , Dinghy.dataSources = convertDataSources
        <$> (Pacrd.dataSources . Pacrd.appSpec $ pacrdApp)
    }
