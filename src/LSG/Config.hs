{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Description : Contains types for reading/writing config file
-- Maintainer  : Preston Gray
module LSG.Config
  ( LSGConfig (..)
  , Options (..)
  , createDefaultConfig
  , getDefaultConfig
  )
where

import Control.Exception (SomeException, catch)
import qualified Data.Text as Text
import qualified Dhall
import GHC.Generics (Generic)
import qualified System.Console.ANSI as ANSI
import qualified System.Directory as System
import System.IO (hPutStrLn, stderr)

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

-- | The location of the default config: ~/.lsgrc
defaultConfigPath :: IO FilePath
defaultConfigPath = do
  home <- System.getHomeDirectory
  return $ home <> "/" <> ".lsgrc"

------------------
-- Write Config --
------------------

-- | Creates the default config file
-- Prompts the user to overwrite if the file already exists
createDefaultConfig :: IO ()
createDefaultConfig = do
  fileExists <- System.doesFileExist =<< defaultConfigPath
  if fileExists
     then promptUser "Config already exists. Overwrite? Y/[n]/(d)isplay contents"
     else writeDefaultConfig

-- | Prompt the user to overwrite the config or display current contents
promptUser :: String -> IO ()
promptUser prompt = do
  putStrLn prompt
  getUserInput

-- | Reads user input and parses the response
getUserInput :: IO ()
getUserInput = (parseResponse . Text.toLower . Text.pack) =<< getLine

-- | Parses the user reply to: Y/[n]/(d)isplay contents
parseResponse :: Text.Text -> IO ()
parseResponse "y" = writeDefaultConfig
parseResponse "d" = displayConfigContents
parseResponse "n" = pure ()
parseResponse "" = pure ()
parseResponse _ = promptUser "Invalid input. Overwrite config? Y/[n]/(d)isplay contents"

-- | Reads and displays the contents of the default config and prompts user again
displayConfigContents :: IO ()
displayConfigContents = do
  contents <- readFile =<< defaultConfigPath
  _ <- sequence $ putStrLn <$> (lines contents)
  promptUser "Overwrite this config? Y/[n]/(d)isplay contents"

-- | Writes the default config file
writeDefaultConfig :: IO ()
writeDefaultConfig = do
  filename <- defaultConfigPath
  writeFile filename defaultConfigContents

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

-----------------
-- Read Config --
-----------------

-- | Reads the config file if it exists, using default false flags if it does not
getDefaultConfig :: IO LSGConfig
getDefaultConfig = do
  filename <- defaultConfigPath
  fileExists <- System.doesFileExist filename
  if fileExists
     then readDefaultConfig filename
     else return defaultConfig

-- | Attempts to read the default config from ~/.lsgrc
-- Uses the default of all false flags if the config file cannot be parsed
readDefaultConfig :: FilePath -> IO LSGConfig
readDefaultConfig filename = do
  catch
    (Dhall.inputFile Dhall.auto filename :: IO LSGConfig)
    (\exc -> do
        let _ = exc :: SomeException
        hPutStrLn stderr ("Warning: Could not parse ~/.lsgrc. Using standard defaults")
        hPutStrLn stderr ("Running `lsg --generate-config` will create a working default config")
        return defaultConfig
    )

-- | The default config for operation if no config file exists
defaultConfig :: LSGConfig
defaultConfig =
  LSGConfig
    { lsgOptions = defaultOptions
    , lsgMatchColor = ANSI.Green
    }
  where
    defaultOptions :: Options
    defaultOptions =
      Options
        { optAll = False
        , optFiles = False
        , optColor = False
        , optHidden = False
        , optVersion = False
        , optCaseMatch = False
        , optStartsWith = False
        , optDirectories = False
        , optExecutables = False
        , optNonExecutables = False
        }

---------------
-- Color ISO --
---------------

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

toANSIColor :: Color -> ANSI.Color
toANSIColor Black = ANSI.Black
toANSIColor Red = ANSI.Red
toANSIColor Green = ANSI.Green
toANSIColor Yellow = ANSI.Yellow
toANSIColor Blue = ANSI.Blue
toANSIColor Magenta = ANSI.Magenta
toANSIColor Cyan = ANSI.Cyan
toANSIColor White = ANSI.White
