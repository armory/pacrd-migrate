{-|
Module      : Pacrd
Description : Types and operations in the PaCRD domain.
Copyright   : (c) Armory.io 2021
License     : BSD3
Maintainer  : Fernando Freire
Stability   : experimental
-}

module Pacrd where

import           Data.Text           (Text)
import           Pacrd.Application   (Application)
import qualified Pacrd.Application   as App
import qualified Pacrd.Internal      as Internal
import           Pacrd.Pipeline      (Pipeline)
import qualified Pacrd.Pipeline      as Pipe

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

type PacrdTree = HashMap Text (Application, [Pipeline])

pacrdTree :: [Application] -> [Pipeline] -> PacrdTree
pacrdTree as ps =
    let
        init = HM.fromList
            $ zip (Internal.name . App.metadata <$> as) (zip as (repeat []))
    in go init ps
  where
    go i (p : []) = insert i p
    go i (p : ps) = go (insert i p) ps

insert :: PacrdTree -> Pipeline -> PacrdTree
insert t p =
    let k = Pipe.application . Pipe.spec $ p
    in
        case HM.lookup k t of
            Nothing     -> t -- NOTE: we're electing to fail silently here under the assumption that Applications _will_ have a manifest
            Just (a, v) -> HM.insert k (a, p : v) t
