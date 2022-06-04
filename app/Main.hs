-- |
-- Description :
--   Parses and validates the CLI args and flags passed.
--   Delegates to the proper routine based on the results.
-- Maintainer  : Preston Gray
module Main where

import qualified LSG.Config as Config
import qualified LSG.OptionParser as Parser
import qualified LSG.System as System
import Options.Applicative ((<**>))
import qualified Options.Applicative as Options

-- | The main entry point for the lsg executable
main :: IO ()
main = do
  config <- Config.getDefaultConfig
  lsgFunction <- Options.execParser $ info config
  case lsgFunction of
    Parser.GenerateConfig -> Config.createDefaultConfig
    Parser.Standard modifiers ->
      either
        (Parser.printError (info config))
        System.listAndGrep
        (Parser.validateModifiers modifiers)
  where
    info :: Config.LSGConfig -> Options.ParserInfo Parser.LSGFunction
    info config = Options.info (Parser.parseFunction config <**> Options.helper)
      ( Options.fullDesc
          <> Options.progDesc
              ( "Search pattern: lsg [OPTIONS] PATTERN | "
                <> "Generate config: lsg --generate-config"
              )
          <> Options.header "lsg - an executable for searching your current directory"
      )
