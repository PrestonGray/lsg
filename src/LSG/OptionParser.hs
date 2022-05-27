{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Contains types and functions for parsing CLI arguments
-- Maintainer  : Preston Gray
module LSG.OptionParser where

import Data.Semigroup ((<>))
import LSG.Config (LSGConfig (..), Options (..))
import Options.Applicative ((<|>))
import qualified Options.Applicative as Opts
import qualified System.Console.ANSI as ANSI

-- | The top level functionality of lsg based on parsed arguments
data LSGFunction
  = Standard Modifiers
  | GenerateConfig
  deriving (Eq, Show)

-- | The parsed arguments for standard lsg function 
data Modifiers = Modifiers
  { lsgPattern :: String
  , lsgConfig :: LSGConfig
  } deriving (Eq, Show)

-- Manually print the error and usage text
printError :: Opts.ParserInfo a -> Opts.ParseError -> IO ()
printError parserInfo parseError =
  Opts.handleParseResult . Opts.Failure $
    Opts.parserFailure Opts.defaultPrefs parserInfo parseError mempty

-- | Helper function creating generic structure for parsing flags
flagParser :: Char -> String -> String -> Bool -> Opts.Parser Bool
flagParser short long help config =
  Opts.flag config (not config)
    ( Opts.long long
      <> Opts.short short
      <> Opts.help help
    )

-- | Top level options parse function for alternative function
parseFunction :: Opts.Parser LSGFunction
parseFunction = parseStandardOptions <|> parseGenerateConfig

-- | Parses generate config function
parseGenerateConfig :: Opts.Parser LSGFunction
parseGenerateConfig =
  Opts.flag' GenerateConfig
    ( Opts.long "generate-config"
      <> Opts.help "Generate a default config at ~/.lsgrc"
    )

-- | Parses the standard argument and option executable call pattern
-- e.g. lsg PATTERN [FLAGS]
parseStandardOptions :: Opts.Parser LSGFunction
parseStandardOptions =
  Standard <$>
    (Modifiers
      <$> (Opts.argument Opts.str (Opts.metavar "PATTERN"))
      <*> (LSGConfig
            <$> (Options
                  <$> allParser
                  <*> fileParser
                  <*> colorParser
                  <*> hiddenParser
                  <*> versionParser
                  <*> caseMatchParser
                  <*> startsWithParser
                  <*> directoryParser
                  <*> executableParser
                  <*> filterExecutableParser
                )
            <*> matchColorParser
          )
    )

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

matchColorParser :: Opts.Parser ANSI.Color
matchColorParser = pure $ lsgMatchColor defaultConfig

-- | TODO: Parse defaults from ~/.lsgrc config
defaultConfig :: LSGConfig
defaultConfig =
  LSGConfig
    { lsgOptions = defaultOptions
    , lsgMatchColor = ANSI.Green
    }

-- | TODO: Parse defaults from ~/.lsgrc config
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
    , optFilterExecutables = False
    }

-- | Ensure that conflicting flags aren't passed
validateModifiers :: Modifiers -> Either Opts.ParseError Modifiers
validateModifiers modifiers@(Modifiers _ config)
  | (optExecutables $ lsgOptions config) && (optFilterExecutables $ lsgOptions config) =
      Left $ Opts.ErrorMsg "Cannot use both (-x|--executables) and (-X|--non-executables)"
  | otherwise = Right modifiers
