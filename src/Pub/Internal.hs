-----------------------------------------------------------------------------
-- |
-- Module      :  StatParse.Internal
-- Copyright   :  (C) 2014 Parnell Springmeyer
-- License     :  AllRightsReserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Internal functions and types for the `Main` program. Mostly
-- responsible for defining the `cmdargs` data type, parsing
-- configuration, and setting the log level.
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pub.Internal where

import           Control.Applicative
import           Data.Maybe
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import           Database.Redis
import           Filesystem.Path.CurrentOS as FS
import           Statistics.Sample
import           System.Console.CmdArgs
import           System.Log.Logger
import           Text.Groom                (groom)

-- | Data type for program CLI options.
data PArgs = PArgs
    { chan :: Maybe String
    , host :: Maybe String
    , port :: Maybe String
    , db   :: Maybe Int
    } deriving (Data, Typeable, Show, Eq)

-- | Data type for settings once merged from CLI.
data Settings = Settings
    { loglevel  :: !Priority
    , channel   :: Maybe String
    , redisHost :: Maybe HostName
    , redisPort :: Maybe PortID
    , redisDB   :: Maybe Int
    } deriving (Show, Eq)

-- | Given options, set the logging level, and create the settings
-- record.
handleOpts :: PArgs -> IO Settings
handleOpts cliopts = do
    whenNormal $ updateGlobalLogger "Console" (setLevel ERROR)
    whenLoud   $ updateGlobalLogger "Console" (setLevel DEBUG)

    let conf = Settings { loglevel  = INFO
                        , channel   = chan cliopts
                        , redisHost = host cliopts
                        , redisPort = fmap PortNumber $ port cliopts
                        , redisDB   = db cliopts
                        }

    debugM "Console" "Configuration parsed"
    debugM "Console" $ groom conf

    return conf
