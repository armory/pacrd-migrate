module Cli where

import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Version              (showVersion)
import           Options.Applicative       (Parser, ParserInfo, ReadM,
                                            execParser, footer, fullDesc,
                                            header, help, helper, info,
                                            infoOption, long, option, progDesc,
                                            short)
import           Options.Applicative.Types (readerAsk)
import qualified Paths_pacrd_migrate       as Paths

-- | Parse an argument as 'Text'.
text :: ReadM Text
text = T.pack <$> readerAsk


migrateVersion :: String
migrateVersion = "pacrd-migrate " <> showVersion Paths.version

parseVersion :: Parser (a -> a)
parseVersion = infoOption migrateVersion $ long "version" <> help
    "Print the current version and exit."

withInfo :: Parser a -> Text -> ParserInfo a
withInfo parser help = info
    (helper <*> parseVersion <*> parser)
    (  fullDesc
    <> progDesc (T.unpack help)
    <> header
           (  migrateVersion
           <> " - simple migration tool from PaCRD to Dinghy file formats."
           )
    <> footer "Copyright (c) Armory, Inc. 2021"
    )

parseInputPath :: Parser Text
parseInputPath = option text $ short 'i' <> long "input-path" <> help
    "The input path were PaCrd manifests can be found."

parseOutputPath :: Parser Text
parseOutputPath = option text $ short 'o' <> long "output-path" <> help
    "The output location to write Dinghyfiles."

data Command = Command { inputPath :: !Text, outputPath :: !Text }

parseCli :: IO Command
parseCli =
    execParser $ (Command <$> parseInputPath <*> parseOutputPath) `withInfo` ""
