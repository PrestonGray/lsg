module Main where

-- import qualified Filters
import qualified OptionParser as Parser
import qualified Options.Applicative as Opts
-- import System.Directory (getCurrentDirectory, listDirectory, makeAbsolute)
-- import System.Environment (getArgs)

main :: IO ()
main = printOptions =<< Opts.execParser opts
  where
    opts = Opts.info (Parser.options Opts.<**> Opts.helper)
      ( Opts.fullDesc
          <> Opts.progDesc "Print the options passed"
          <> Opts.header "hello - a test for optparse-applicative"
      )

printOptions :: Parser.Options -> IO ()
printOptions options = print options

-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     [] -> putStrLn "USAGE: lsg [PATTERN]"
--     [pattern] -> do
--       files <- listDirectory =<< getCurrentDirectory
--       let filtered = filter (\file -> Filters.substring pattern file) files
--       print filtered

