module Main where

import System.Directory (getCurrentDirectory, listDirectory, makeAbsolute)
import System.Environment (getArgs)
import qualified Filters

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "USAGE: lsg [PATTERN]"
    [pattern] -> do
      files <- listDirectory =<< getCurrentDirectory
      let filtered = filter (\file -> Filters.substring pattern file) files
      print filtered

