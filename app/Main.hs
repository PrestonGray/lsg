-- |
-- Description :
--   Parses and validates the CLI args and flags passed.
--   Delegates to the proper routine based on the results.
-- Maintainer  : Preston Gray
module Main where

import qualified LSG.OptionParser as Parser
import qualified LSG.System as System
import Options.Applicative ((<**>))
import qualified Options.Applicative as Options

-- | The main entry point for the lsg executable
main :: IO ()
main = do
  lsgFunction <- Options.execParser info
  case lsgFunction of
    Parser.GenerateConfig -> System.generateConfig
    Parser.Standard modifiers ->
      either
        (Parser.printError info)
        System.listAndGrep
        (Parser.validateModifiers modifiers)
  where
    info = Options.info (Parser.parseFunction <**> Options.helper)
      ( Options.fullDesc
          <> Options.progDesc
              ( "Search pattern: lsg [OPTIONS] PATTERN | "
                <> "Generate config: lsg --generate-config"
              )
          <> Options.header "lsg - an executable for searching your current directory"
      )
