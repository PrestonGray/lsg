-- |
-- Description :
--   System exposes the entry point for operation of lsg, searching the 
--   current directory for a pattern filtered by passed options.
-- Maintainer  : Preston Gray
module LSG.System (listAndGrep) where

import qualified LSG.Filter as Filter
import qualified LSG.OptionParser as Opts
import System.Directory (getCurrentDirectory, listDirectory)

-- | The standard routine for lsg
-- Searches for the target pattern and filters the results based on flags
-- USAGE: lsg PATTERN [FLAGS]
-- TODO: Implement different function based on options
listAndGrep :: Opts.Modifiers -> IO ()
listAndGrep opts = do
  files <- listDirectory =<< getCurrentDirectory
  let filtered = filter (\file -> Filter.substring (Opts.lsgPattern opts) file) files
  print filtered
