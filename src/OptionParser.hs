{-# LANGUAGE OverloadedStrings #-}

module OptionParser where

import Data.Semigroup ((<>))
import qualified Options.Applicative as Opts

-- | Options outlines possible command line arguments
data Options = Options
  { optAll :: Flag
  , optHelp :: Flag
  , optColor :: Flag
  , optHidden :: Flag
  , optVersion :: Flag
  , optCaseMatch :: Flag
  , optStartsWith :: Flag
  , optExecutables :: Flag
  , optFilterExecutables :: Flag
  -- , optResultFilter :: Maybe ResultFilter
  } deriving (Eq, Show)

-- | ResultFilter indicates type of results shown
-- data ResultFilter = Files | Directories

-- | Status indicates if an option is enabled or disabled
data Flag = Enabled | Disabled
  deriving (Eq, Show)

-- | Inverts the Flag state of an option
invertFlag :: Flag -> Flag
invertFlag Enabled  = Disabled
invertFlag Disabled = Enabled

options :: Opts.Parser Options
options =
  Options
    <$> allParser
    <*> helpParser
    <*> colorParser
    <*> hiddenParser
    <*> versionParser
    <*> caseMatchParser
    <*> startsWithParser
    <*> executableParser
    <*> filterExecutableParser

allParser :: Opts.Parser Flag
allParser =
  flagParser
    'a'
    "all"
    "Search all files, including hidden files"
    (optAll readDefaultOptions)

helpParser :: Opts.Parser Flag
helpParser =
  flagParser
    'h'
    "help"
    "Print the help for lsg"
    (optHelp readDefaultOptions)

colorParser :: Opts.Parser Flag
colorParser =
  flagParser
    'c'
    "color"
    "Enable/disable color. Overrides config"
    (optColor readDefaultOptions)

hiddenParser :: Opts.Parser Flag
hiddenParser =
  flagParser
    'o'
    "hidden"
    "Search only hidden files"
    (optHidden readDefaultOptions)

versionParser :: Opts.Parser Flag
versionParser =
  flagParser
    'v'
    "version"
    "Print version info"
    (optVersion readDefaultOptions)

caseMatchParser :: Opts.Parser Flag
caseMatchParser =
  flagParser
    'm'
    "match-case"
    "Enable/Disable case-sensitive matches. Overrides config"
    (optCaseMatch readDefaultOptions)

startsWithParser :: Opts.Parser Flag
startsWithParser =
  flagParser
    's'
    "starts-with"
    "Return matches that start with the pattern"
    (optStartsWith readDefaultOptions)

executableParser :: Opts.Parser Flag
executableParser =
  flagParser
    'x'
    "executables"
    "Search only executable files"
    (optExecutables readDefaultOptions)

filterExecutableParser :: Opts.Parser Flag
filterExecutableParser =
  flagParser
    'X'
    "non-executables"
    "Filter out executables from search results"
    (optFilterExecutables readDefaultOptions)

-- | Generic parse structure for flags
flagParser :: Char -> String -> String -> Flag -> Opts.Parser Flag
flagParser short long help config =
  Opts.flag config (invertFlag config)
    ( Opts.long long
      <> Opts.short short
      <> Opts.help help
    )

-- | Directory flag parser
-- directoryParser :: Opts.Parser Flag
-- directoryParser =
--   flagParser
--     'd'
--     "directories"
--     "Search only directories"
--     (optDirectories readDefaultOptions)

-- | File flag parser
-- fileParser :: Opts.Parser Flag
-- fileParser =
--   flagParser
--     'f'
--     "files"
--     "Search only files"
--     (optFiles readDefaultOptions)

--   , Option ['s'] ["starts-with"]
--       (NoArg (\opts -> opts { _optStartsWith = Enabled }))
--       "Return matches that start with the pattern"
--   , Option ['x'] ["executables"]
--       (NoArg (\opts -> opts { _optExecutables = Enabled }))
--       "Search only executable files"
--   , Option ['X'] ["non-executables"]
--       (NoArg (\opts -> opts { _optFilterExecutables = Enabled }))
--       "Filter out executable files"
--   ]

-- TODO: Read defaults from ~/.lsgrc and memoize somehow
readDefaultOptions :: Options
readDefaultOptions =
  Options
    { optAll = Disabled
    , optHelp = Disabled
    , optColor = Disabled
    , optHidden = Disabled
    , optVersion = Disabled
    , optCaseMatch = Disabled
    , optStartsWith = Disabled
    , optExecutables = Disabled
    , optFilterExecutables = Disabled
    -- , optResultFilter = Nothing
    }

-- | TODO: validateOptions will make sure that conflicting flags aren't passed
-- validateOptions :: Options -> Either Text ()
-- validateOptions
