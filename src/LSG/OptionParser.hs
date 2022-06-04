{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description : Contains types and functions for parsing CLI arguments
-- Maintainer  : Preston Gray
module LSG.OptionParser where

import Data.Semigroup ((<>))
import LSG.Config (LSGConfig (..), Options (..))
import Options.Applicative ((<|>))
import qualified Options.Applicative as Opts

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
parseFunction :: LSGConfig -> Opts.Parser LSGFunction
parseFunction cfg = (parseStandardOptions cfg) <|> parseGenerateConfig

-- | Parses generate config function
parseGenerateConfig :: Opts.Parser LSGFunction
parseGenerateConfig =
  Opts.flag' GenerateConfig
    ( Opts.long "generate-config"
      <> Opts.help "Generate an editable default config at ~/.lsgrc"
    )

-- | Parses the standard argument and option executable call pattern
-- e.g. lsg PATTERN [FLAGS]
parseStandardOptions :: LSGConfig -> Opts.Parser LSGFunction
parseStandardOptions (LSGConfig opts color) =
  Standard <$>
    (Modifiers
      <$> (Opts.argument Opts.str (Opts.metavar "PATTERN"))
      <*> (LSGConfig
            <$> (Options
                  <$> (allParser $ optAll opts)
                  <*> (fileParser $ optFiles opts)
                  <*> (directoryParser $ optDirectories opts)
                  <*> (hiddenParser $ optHidden opts)
                  <*> (executableParser $ optExecutables opts)
                  <*> (filterExecutableParser $ optNonExecutables opts)
                  <*> (caseMatchParser $ optCaseMatch opts)
                  <*> (startsWithParser $ optStartsWith opts)
                  <*> (colorParser $ optColor opts)
                  <*> versionParser
                )
              <*> (pure color)
          )
    )

allParser :: Bool -> Opts.Parser Bool
allParser defaultOpt =
  flagParser
    'a'
    "all"
    "Search all files, including hidden files"
    defaultOpt

colorParser :: Bool -> Opts.Parser Bool
colorParser defaultOpt =
  flagParser
    'c'
    "color"
    "Enable/disable color. Overrides config"
    defaultOpt

hiddenParser :: Bool -> Opts.Parser Bool
hiddenParser defaultOpt =
  flagParser
    'o'
    "hidden"
    "Search only hidden files"
    defaultOpt

caseMatchParser :: Bool -> Opts.Parser Bool
caseMatchParser defaultOpt =
  flagParser
    'm'
    "match-case"
    "Enable/Disable case-sensitive matches. Overrides config"
    defaultOpt

startsWithParser :: Bool -> Opts.Parser Bool
startsWithParser defaultOpt =
  flagParser
    's'
    "starts-with"
    "Return matches that start with the pattern"
    defaultOpt

executableParser :: Bool -> Opts.Parser Bool
executableParser defaultOpt =
  flagParser
    'x'
    "executables"
    "Search only executable files"
    defaultOpt

filterExecutableParser :: Bool -> Opts.Parser Bool
filterExecutableParser defaultOpt =
  flagParser
    'X'
    "non-executables"
    "Filter out executables from search results"
    defaultOpt

directoryParser :: Bool -> Opts.Parser Bool
directoryParser defaultOpt =
  flagParser
    'd'
    "directories"
    "Search only directories"
    defaultOpt

fileParser :: Bool -> Opts.Parser Bool
fileParser defaultOpt =
  flagParser
    'f'
    "files"
    "Search only files"
    defaultOpt

-- | The version flag is a simple boolean switch as it is not configurable
versionParser :: Opts.Parser Bool
versionParser =
  Opts.switch
    ( Opts.long "version"
        <> Opts.short 'V'
        <> Opts.help "Print lsg version"
    )

-- | Ensure that conflicting flags aren't passed
validateModifiers :: Modifiers -> Either Opts.ParseError Modifiers
validateModifiers modifiers@(Modifiers _ config)
  | (optExecutables $ lsgOptions config) && (optNonExecutables $ lsgOptions config) =
      Left $ Opts.ErrorMsg "Cannot use both (-x|--executables) and (-X|--non-executables)"
  | otherwise = Right modifiers
