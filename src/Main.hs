{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Cli              (Command (..), inputPath, outputPath)
import qualified Cli
import           Control.Monad    (join)
import qualified Data.Text        as T
import qualified Migrate
import           Pacrd
import qualified System.Directory as Dir
import           Wrapper

main :: IO ()
main = do
    Command { inputPath, outputPath } <- Cli.parseCli
    files <- filter isYamlFile <$> Dir.listDirectory (T.unpack inputPath)
    manifests <- join <$> mapM readManifest files
    let (apps, pipes) = splitWrapper manifests
    print . Migrate.convertPacrdTree $ pacrdTree apps pipes
    -- TODO
    putStrLn $ "Writing Dinghyfile to ... '" <> T.unpack outputPath <> "'"
