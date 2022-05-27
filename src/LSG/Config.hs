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

instance Dhall.ToDhall LSGConfig where
  injectWith _ =
    Dhall.recordEncoder $
      adapt
        >$< Dhall.encodeField "options"
        >*< Dhall.encodeField "color"
    where
      adapt LSGConfig {..} = (lsgOptions, fromANSIColor lsgMatchColor)

-- | Options outlines possible command line arguments
-- All options default to False and are configurable in ~/.lsgrc, excluding version
data Options = Options
  { optAll :: Bool
  , optFiles :: Bool
  , optColor :: Bool
  , optHidden :: Bool
  -- Version is not configurable
  , optVersion :: Bool
  , optCaseMatch :: Bool
  , optStartsWith :: Bool
  , optDirectories :: Bool
  , optExecutables :: Bool
  , optFilterExecutables :: Bool
  } deriving (Eq, Show)

instance Dhall.FromDhall Options where
  autoWith _ =
    Dhall.record $
      Options
        <$> Dhall.field "searchAll" Dhall.auto
        <*> Dhall.field "matchFilesOnly" Dhall.auto
        <*> Dhall.field "useColor" Dhall.auto
        <*> Dhall.field "matchHiddenOnly" Dhall.auto
        <*> pure False
        <*> Dhall.field "caseSensitive" Dhall.auto
        <*> Dhall.field "matchBeginning" Dhall.auto
        <*> Dhall.field "matchDirectoriesOnly" Dhall.auto
        <*> Dhall.field "matchExecutablesOnly" Dhall.auto
        <*> Dhall.field "matchNonExecutablesOnly" Dhall.auto

instance Dhall.ToDhall Options where
  injectWith _ =
    Dhall.recordEncoder $
      adapt
        >$< Dhall.encodeField "searchAll"
        >*< Dhall.encodeField "matchFilesOnly"
        >*< Dhall.encodeField "useColor"
        >*< Dhall.encodeField "matchHiddenOnly"
        >*< Dhall.encodeField "caseSensitive"
        >*< Dhall.encodeField "matchBeginning"
        >*< Dhall.encodeField "matchDirectoriesOnly"
        >*< Dhall.encodeField "matchExecutablesOnly"
        >*< Dhall.encodeField "matchNonExecutablesOnly"
    where adapt Options {..} = (optAll, (optFiles, (optColor, (optHidden, (optCaseMatch, (optStartsWith, (optDirectories, (optExecutables, optFilterExecutables))))))))

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

instance Dhall.ToDhall Color

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
