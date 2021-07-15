{-|
Module      : Wrapper
Description : Types and operations for reading YAML documents in this domain.
Copyright   : (c) Armory.io 2021
License     : BSD3
Maintainer  : Fernando Freire
Stability   : experimental
-}

module Wrapper where

import qualified Data.List         as List
import           Data.Text         (Text)
import           Data.Yaml         (FromJSON, parseJSON, withObject, (.:))
import qualified Data.Yaml         as Yaml
import qualified Pacrd.Application as PacApp
import qualified Pacrd.Pipeline    as PacPipe

data Wrapper = A PacApp.Application | P PacPipe.Pipeline

instance FromJSON Wrapper where
  parseJSON = withObject "Wrapper" $ \o -> do
    kind <- (o .: "kind") :: Yaml.Parser Text
    case kind of
      "Application" ->
        A <$> ((parseJSON $ Yaml.Object o) :: Yaml.Parser PacApp.Application)
      "Pipeline" ->
        P <$> ((parseJSON $ Yaml.Object o) :: Yaml.Parser PacPipe.Pipeline)
      _ -> fail "oops" -- TODO

-- | Effectively the `either` function for Either a b, but for Wrapper
unrollWrapper :: (Wrapper -> c) -> (Wrapper -> c) -> Wrapper -> c
unrollWrapper f _ a@(A _) = f a
unrollWrapper _ g p@(P _) = g p

-- | Effectively a partition of an Either, but for Wrapper
splitWrapper :: [Wrapper] -> ([PacApp.Application], [PacPipe.Pipeline])
splitWrapper = foldr (unrollWrapper app pipe) ([], [])
  where
    app (A a) ~(l, r) = (a : l, r)
    pipe (P a) ~(l, r) = (l, a : r)

-- | Read a manifest file from the given path.
readManifest :: FilePath -> IO [Wrapper]
readManifest path = do
    result <- Yaml.decodeAllFileEither path
    case result of
        Left  err -> fail $ "could not read '" ++ path ++ "': " ++ show err
        Right val -> return val

isYamlFile :: FilePath -> Bool
isYamlFile path = List.isSuffixOf ".yaml" path || List.isSuffixOf ".yml" path
