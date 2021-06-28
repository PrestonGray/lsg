{-# LANGUAGE OverloadedStrings #-}

module Types where

import System.Console.GetOpt

data Options =
  Options
    { _optAll :: Status
    , _optHelp :: Status
    , _optColor :: Status
    , _optHidden :: Status
    , _optVersion :: Status
    , _optMatchCase :: Status
    , _optExecutables :: Status
    , _optNonExecutables :: Status
    , _optResultFilter :: Maybe ResultFilter
    } deriving (Eq, Show)

data ResultFilter =
    Files
  | Directories
  deriving (Eq, Show)

data Status = Enabled | Disabled
  deriving (Eq, Show)

inverse :: Status -> Status
inverse Enabled  = Disabled
inverse Disabled = Enabled

-- TODO: Read defaults from ~/.lsgrc
readDefaultOptions :: Options
readDefaultOptions =
  Options
    { _optAll = Disabled
    , _optHelp = Disabled
    , _optColor = Disabled
    , _optHidden = Disabled
    , _optVersion = Disabled
    , _optMatchCase = Disabled
    , _optExecutables = Disabled
    , _optNonExecutables = Disabled
    , _optResultFilter = Nothing
    }

options :: Options -> [OptDescr (Options -> Options)]
options defaults =
  [ Option ['a'] ["all"]
      (NoArg (\opts -> opts { _optAll = Enabled }))
      "Searchs all files, including hidden files"
  , Option ['c'] ["color"]
      (NoArg (\opts -> opts { _optColor = (inverse $ _optColor defaults) }))
      "Enable/disable color. Overrides config"
  , Option ['d'] ["directories"]
      (NoArg (\opts -> opts { _optResultFilter = Just Directories }))
      "Search only directories"
  , Option ['f'] ["files"]
      (NoArg (\opts -> opts { _optResultFilter = Just Files }))
      "Search only files"
  , Option ['h'] ["help"]
      (NoArg (\opts -> opts { _optHelp = Enabled }))
      "Print the help"
  , Option ['m'] ["match-case"]
      (NoArg (\opts -> opts { _optMatchCase = (inverse $ _optMatchCase defaults) }))
      "Enable/Disable case-sensitive matches. Overrides config"
  , Option ['o'] ["hidden"]
      (NoArg (\opts -> opts { _optHidden = Enabled }))
      "Search only hidden files"
  , Option ['v'] ["version"]
      (NoArg (\opts -> opts { _optVersion = Enabled }))
      "Print version info"
  , Option ['x'] ["executables"]
      (NoArg (\opts -> opts { _optExecutables = Enabled }))
      "Search only executable files"
  , Option ['X'] ["non-executables"]
      (NoArg (\opts -> opts { _optNonExecutables = Enabled }))
      "Search only non-executable files"
  ]
