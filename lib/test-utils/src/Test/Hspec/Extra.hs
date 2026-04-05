{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Helper functions for testing.
--

module Test.Hspec.Extra
    ( aroundAll
    , hspecMain
    ) where

import Prelude

import Control.Monad
    ( (<=<)
    )
import Data.List
    ( elemIndex
    )
-- See ADP-1910
import "optparse-applicative" Options.Applicative
    ( Parser
    , ParserInfo (..)
    , ReadM
    , eitherReader
    , execParser
    , failureCode
    , forwardOptions
    , help
    , info
    , long
    , many
    , metavar
    , option
    , short
    , strArgument
    )
import System.Environment
    ( withArgs
    )
import Test.Hspec
    ( ActionWith
    , HasCallStack
    , Spec
    , SpecWith
    , afterAll
    , beforeAll
    , beforeWith

    )
import Test.Hspec.Core.Runner
    ( Config (..)
    , Summary
    , defaultConfig
    , evaluateSummary
    , hspecWithResult
    )
import Test.Utils.Env
    ( withAddedEnv
    )
import Test.Utils.Resource
    ( unBracket
    )
import Test.Utils.Startup
    ( withLineBuffering
    )

-- | Run a 'bracket' resource acquisition function around all the specs. The
-- resource is allocated just before the first test case and released
-- immediately after the last test case.
--
-- Each test is given the resource as a function parameter.
aroundAll
    :: forall a. HasCallStack
    => (ActionWith a -> IO ())
    -> SpecWith a
    -> Spec
aroundAll acquire =
    beforeAll (unBracket acquire) . afterAll snd . beforeWith fst

-- | Add execution timing information to test output.
--
configWithExecutionTimes :: Config -> Config
configWithExecutionTimes config = config
    { configPrintCpuTime = True
      -- Prints the total elapsed CPU time for the entire test suite.
    , configPrintSlowItems = Just 10
      -- Prints a list of the slowest tests in descending order of
      -- elapsed CPU time.
    , configTimes = True
      -- Appends the elapsed CPU time to the end of each individual test.
    }

{-------------------------------------------------------------------------------
                             Test suite runner main
-------------------------------------------------------------------------------}

-- | Main function for running a test suite using 'getDefaultConfig'.
hspecMain :: Spec -> IO ()
hspecMain = hspecMain' getDefaultConfig

-- | An IO action that runs around 'hspecWith'.
type HspecWrapper a = IO Summary -> IO a

-- | Main function for running a test suite. Like 'Test.Hspec.hspec', except it
-- allows for a custom action to modify the environment and configuration before
-- passing control over to Hspec.
hspecMain' :: IO (HspecWrapper a, Config) -> Spec -> IO a
hspecMain' getConfig spec = withLineBuffering $ do
    (wrapper, config) <- getConfig
    wrapper $ hspecWithResult config spec

-- | Our custom Hspec wrapper. It adds the @--env@ option for setting
-- environment variables, and prints the tests which took the longest time after
-- finishing the test suite.
getDefaultConfig :: IO (HspecWrapper (), Config)
getDefaultConfig = do
    (env, args) <- execParser setEnvParser
    pure ( evaluateSummary <=< withArgs args . withAddedEnv env
         , configWithExecutionTimes defaultConfig)

-- | A CLI arguments parser which handles setting environment variables.
setEnvParser :: ParserInfo ([(String, String)], [String])
setEnvParser = info ((,) <$> many setEnvOpt <*> restArgs) $
    forwardOptions <> failureCode 89
  where
    setEnvOpt :: Parser (String, String)
    setEnvOpt = option readSetEnv
            (  long "env"
            <> short 'e'
            <> metavar "NAME=VALUE"
            <> help "Export the given environment variable to the test suite" )

    readSetEnv :: ReadM (String, String)
    readSetEnv = eitherReader $ \arg -> case elemIndex '=' arg of
        Just i | i > 0 -> Right (take i arg, drop (i+1) arg)
        _ -> Left "does not match syntax NAME=VALUE"

    restArgs :: Parser [String]
    restArgs = many $ strArgument (metavar "HSPEC-ARGS...")
