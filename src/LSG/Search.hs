-- |
-- Description : The functions for filtering results based on passed options
-- Maintainer  : Preston Gray
module LSG.Search where

import Data.List (isInfixOf, isPrefixOf)
import LSG.Config (Options (..))
import qualified System.Directory as System

searchDirectory :: Options -> String -> IO [FilePath]
searchDirectory opts pattern = do
  -- Get all the files/directories in the current directory
  targets <- System.listDirectory =<< System.getCurrentDirectory
  return $ searchTargets opts pattern targets

-- | Filter the list of targets by those that match the pattern
searchTargets :: Options -> String -> [FilePath] -> [FilePath]
searchTargets opts
  | optStartsWith opts = matchTargets isPrefixOf
  | otherwise = matchTargets isInfixOf

-- | Filters files using passed predicate
matchTargets :: ([Char] -> [Char] -> Bool) -> String -> [FilePath] -> [FilePath]
matchTargets matchFunction pattern targets = filter (matchFunction pattern) targets
