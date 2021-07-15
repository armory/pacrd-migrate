module Main where

import           Control.Monad    (join)
import qualified Migrate
import           Pacrd
import qualified System.Directory as Dir
import           Wrapper

main :: IO ()
main = do
    -- TODO support input paths
    files     <- filter isYamlFile <$> Dir.listDirectory "."
    manifests <- join <$> mapM readManifest files
    let (apps, pipes) = splitWrapper manifests
    print . Migrate.convertPacrdTree $ pacrdTree apps pipes
    -- TODO support output paths
