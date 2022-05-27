{-# LANGUAGE OverloadedStrings #-}

module LSG.OptionParser where

import Data.Semigroup ((<>))
import qualified Options.Applicative as Opts

-- | Options outlines possible command line arguments
data Options = Options
  { optPattern :: String
  , optAll :: Bool
  , optFiles :: Bool
  , optColor :: Bool
  , optHidden :: Bool
  , optVersion :: Bool
  , optCaseMatch :: Bool
  , optStartsWith :: Bool
  , optDirectories :: Bool
  , optExecutables :: Bool
  , optFilterExecutables :: Bool
  } deriving (Eq, Show)

-- Manually print the error and usage text
printError :: Opts.ParserInfo a -> Opts.ParseError -> IO ()
printError parserInfo parseError = Opts.handleParseResult $ Opts.Failure
  $ Opts.parserFailure Opts.defaultPrefs parserInfo parseError mempty

-- | Helper function creating generic structure for parsing flags
flagParser :: Char -> String -> String -> Bool -> Opts.Parser Bool
flagParser short long help config =
  Opts.flag config (not config)
    ( Opts.long long
      <> Opts.short short
      <> Opts.help help
    )

-- | Parses the standard argument and option executable call pattern
-- e.g. lsg [FLAGS] PATTERN
parseOptions :: Opts.Parser Options
parseOptions =
  Options
    <$> (Opts.argument Opts.str (Opts.metavar "PATTERN"))
    <*> allParser
    <*> fileParser
    <*> colorParser
    <*> hiddenParser
    <*> versionParser
    <*> caseMatchParser
    <*> startsWithParser
    <*> directoryParser
    <*> executableParser
    <*> filterExecutableParser

allParser :: Opts.Parser Bool
allParser =
  flagParser
    'a'
    "all"
    "Search all files, including hidden files"
    (optAll defaultOptions)

colorParser :: Opts.Parser Bool
colorParser =
  flagParser
    'c'
    "color"
    "Enable/disable color. Overrides config"
    (optColor defaultOptions)

hiddenParser :: Opts.Parser Bool
hiddenParser =
  flagParser
    'o'
    "hidden"
    "Search only hidden files"
    (optHidden defaultOptions)

caseMatchParser :: Opts.Parser Bool
caseMatchParser =
  flagParser
    'm'
    "match-case"
    "Enable/Disable case-sensitive matches. Overrides config"
    (optCaseMatch defaultOptions)

startsWithParser :: Opts.Parser Bool
startsWithParser =
  flagParser
    's'
    "starts-with"
    "Return matches that start with the pattern"
    (optStartsWith defaultOptions)

executableParser :: Opts.Parser Bool
executableParser =
  flagParser
    'x'
    "executables"
    "Search only executable files"
    (optExecutables defaultOptions)

filterExecutableParser :: Opts.Parser Bool
filterExecutableParser =
  flagParser
    'X'
    "non-executables"
    "Filter out executables from search results"
    (optFilterExecutables defaultOptions)

directoryParser :: Opts.Parser Bool
directoryParser =
  flagParser
    'd'
    "directories"
    "Search only directories"
    (optDirectories defaultOptions)

fileParser :: Opts.Parser Bool
fileParser =
  flagParser
    'f'
    "files"
    "Search only files"
    (optFiles defaultOptions)

-- | The version flag is a simple boolean switch as it is not configurable
versionParser :: Opts.Parser Bool
versionParser =
  Opts.switch
    ( Opts.long "version"
        <> Opts.short 'V'
        <> Opts.help "Print lsg version"
    )

-- TODO: Read defaults from ~/.lsgrc and memoize somehow
defaultOptions :: Options
defaultOptions =
  Options
    { optPattern = ""
    , optAll = False
    , optFiles = False
    , optColor = False
    , optHidden = False
    , optVersion = False
    , optCaseMatch = False
    , optStartsWith = False
    , optDirectories = False
    , optExecutables = False
    , optFilterExecutables = False
    }

-- | Ensure that conflicting flags aren't passed
validateOptions :: Options -> Either Opts.ParseError Options
validateOptions opts
  | (optExecutables opts) && (optFilterExecutables opts) =
      Left $ Opts.ErrorMsg "Cannot use both (-x|--executables) and (-X|--non-executables)"
  | otherwise = Right opts
