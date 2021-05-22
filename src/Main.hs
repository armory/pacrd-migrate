module Main where

import           Control.Applicative ((<|>))
import           Control.Monad       (forM_)
import qualified Data.Aeson          as Json
import qualified Data.List           as List
import           Data.Yaml           (FromJSON)
import qualified Data.Yaml           as Yaml
import qualified Migrate
import qualified Pacrd
import qualified System.Directory    as Dir

-- | Read a manifest file from the given path.
readManifest :: (FromJSON a) => FilePath -> IO a
readManifest path = do
    result <- Yaml.decodeFileEither path
    case result of
        Left  err -> fail $ "could not read '" ++ path ++ "': " ++ show err
        Right val -> return val

readPipeline :: FilePath -> IO Pacrd.Pipeline
readPipeline = readManifest

readApp :: FilePath -> IO Pacrd.Application
readApp = readManifest

isYamlFile :: FilePath -> Bool
isYamlFile path = List.isSuffixOf ".yaml" path || List.isSuffixOf ".yml" path

processPipeline :: FilePath -> IO ()
processPipeline f = readPipeline f >>= print

processApplication :: FilePath -> IO ()
processApplication f = readApp f >>= \a -> do
    putStrLn "!!!!!!!!!!!!!!!!!!!!!!"
    putStrLn "Converting Application"
    let a' = Migrate.convertApp a
    print a'
    print . Json.encode $ a'
    putStrLn "!!!!!!!!!!!!!!!!!!!!!!"

main :: IO ()
main = do
    -- TODO support input paths
    files <- filter isYamlFile <$> Dir.listDirectory "."
    forM_ files $ \f -> processPipeline f <|> processApplication f
