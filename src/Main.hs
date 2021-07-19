{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Cli              (Command (..), inputPath, outputPath)
import qualified Cli
import           Control.Monad    (join)
import Data.Foldable (traverse_)
import qualified Data.Text        as T
import qualified Migrate
import           Pacrd
import qualified Dinghy
import qualified System.Directory as Dir
import           Wrapper

main :: IO ()
main = do
    -- TODO consider adding logging here
    Command { inputPath, outputPath } <- Cli.parseCli
    files <- filter isYamlFile <$> Dir.listDirectory (T.unpack inputPath)
    manifests <- join <$> mapM readManifest files
    let (apps, pipes) = splitWrapper manifests
    (flip traverse_) (Migrate.convertPacrdTree $ pacrdTree apps pipes) $ \d ->
      Dinghy.writeDinghyfile outputPath d
