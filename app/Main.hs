module Main where

import qualified LSG.OptionParser as Parser
import qualified LSG.System as System
import Options.Applicative ((<**>))
import qualified Options.Applicative as Options

main :: IO ()
main = do
  eOptions <- Options.execParser info
  case Parser.validateOptions eOptions of
    Left parseError -> Parser.printError info parseError
    Right options -> System.listAndGrep options
  where
    info = Options.info (Parser.parseOptions <**> Options.helper)
      ( Options.fullDesc
          <> Options.progDesc "Search your current directory for a pattern (i.e. ls | grep PATTERN)"
          <> Options.header "lsg - an executable for searching your current directory"
      )
