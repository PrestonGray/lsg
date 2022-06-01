{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Description : Contains types for reading/writing to config file
-- Maintainer  : Preston Gray
module LSG.Config where

import Dhall ((>$<), (>*<))
import qualified Dhall
import GHC.Generics (Generic)
import qualified System.Console.ANSI as ANSI
import qualified System.Directory as System

-- | The options configurable from ~/.lsgrc
data LSGConfig = LSGConfig
  { lsgOptions :: Options
  , lsgMatchColor :: ANSI.Color
  } deriving (Eq, Show, Generic)

instance Dhall.FromDhall LSGConfig where
  autoWith _ =
    Dhall.record $
      LSGConfig
        <$> Dhall.field "options" Dhall.auto
        <*> Dhall.field "color" (fmap toANSIColor (Dhall.auto :: Dhall.Decoder Color))

-- | Options outlines possible command line arguments
-- All options default to False and are configurable in ~/.lsgrc, excluding version
data Options = Options
  { optAll :: Bool
  , optFiles :: Bool
  , optDirectories :: Bool
  , optHidden :: Bool
  , optExecutables :: Bool
  , optNonExecutables :: Bool
  , optCaseMatch :: Bool
  , optStartsWith :: Bool
  , optColor :: Bool
  -- Version is not configurable
  , optVersion :: Bool
  } deriving (Eq, Show)

instance Dhall.FromDhall Options where
  autoWith _ =
    Dhall.record $
      Options
        <$> Dhall.field "searchAll" Dhall.auto
        <*> Dhall.field "matchFilesOnly" Dhall.auto
        <*> Dhall.field "matchDirectoriesOnly" Dhall.auto
        <*> Dhall.field "matchHiddenOnly" Dhall.auto
        <*> Dhall.field "matchExecutablesOnly" Dhall.auto
        <*> Dhall.field "matchNonExecutablesOnly" Dhall.auto
        <*> Dhall.field "caseSensitive" Dhall.auto
        <*> Dhall.field "matchBeginning" Dhall.auto
        <*> Dhall.field "useColor" Dhall.auto
        <*> pure False

-- | An isomorphism for ANSI.Color to prevent an orphan FromDhall instance
data Color
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  deriving (Eq, Show, Generic)

instance Dhall.FromDhall Color

fromANSIColor :: ANSI.Color -> Color
fromANSIColor ANSI.Black = Black
fromANSIColor ANSI.Red = Red
fromANSIColor ANSI.Green = Green
fromANSIColor ANSI.Yellow = Yellow
fromANSIColor ANSI.Blue = Blue
fromANSIColor ANSI.Magenta = Magenta
fromANSIColor ANSI.Cyan = Cyan
fromANSIColor ANSI.White = White

toANSIColor :: Color -> ANSI.Color
toANSIColor Black = ANSI.Black
toANSIColor Red = ANSI.Red
toANSIColor Green = ANSI.Green
toANSIColor Yellow = ANSI.Yellow
toANSIColor Blue = ANSI.Blue
toANSIColor Magenta = ANSI.Magenta
toANSIColor Cyan = ANSI.Cyan
toANSIColor White = ANSI.White

-- | Creates the default ~/.lsgrc config file
-- Prompts the user to overwrite if the file already exists
createDefaultConfig :: IO ()
createDefaultConfig = do
  home <- System.getHomeDirectory
  let filename = ".lsgrc"
  fileExists <- System.doesFileExist (home <> "/" <> filename)
  if fileExists
     then putStrLn "File exists"
     else
      writeFile
        filename
        defaultConfigContents

-- | The default configuration file contents to write
defaultConfigContents :: String
defaultConfigContents =
  "-- Possible color options\n"
  <> "let Color : Type = < Black | Red | Green | Yellow | Blue | Magenta | Cyan | White >\n"
  <> "\n"
  <> "in  { options =\n"
  <> "       {\n"
  <> "         -- Match all file types, including hidden files\n"
  <> "         searchAll = False\n"
  <> "         -- Match files only\n"
  <> "       , matchFilesOnly = False\n"
  <> "         -- Match directories only\n"
  <> "       , matchDirectoriesOnly = False\n"
  <> "         -- Match hidden files (.*) only\n"
  <> "       , matchHiddenOnly = False\n"
  <> "         -- Match executable files only\n"
  <> "         -- NOTE: Mutually exclusive with matchNonExecutablesOnly\n"
  <> "       , matchExecutablesOnly = False\n"
  <> "         -- Match non-executable files only\n"
  <> "         -- NOTE: Mutually exclusive with matchExecutablesOnly\n"
  <> "       , matchNonExecutablesOnly = False\n"
  <> "         -- Enable case sensitive search\n"
  <> "       , caseSensitive = False\n"
  <> "         -- Match files that begin with the pattern\n"
  <> "       , matchBeginning = False\n"
  <> "         -- Color the matched pattern in the output\n"
  <> "       , useColor = False\n"
  <> "       }\n"
  <> "     -- Specify the output color\n"
  <> "   , color = Color.Green\n"
  <> "   }"
