-- |
-- Description :
--   System exposes the entry point for operation of lsg, searching the 
--   current directory for a pattern filtered by passed options.
-- Maintainer  : Preston Gray
module LSG.System (listAndGrep) where

import qualified LSG.Config as Config
import qualified LSG.OptionParser as Opts
import qualified LSG.Search as Search

-- | The standard routine for lsg
-- Searches for the target pattern and filters the results based on flags
-- USAGE: lsg PATTERN [FLAGS]
-- TODO: Implement different function based on options
listAndGrep :: Opts.Modifiers -> IO ()
listAndGrep mods = do
  -- Search the files with the given options and pattern
  results <- Search.searchDirectory (Config.lsgOptions $ Opts.lsgConfig mods) (Opts.lsgPattern mods)
  print results
