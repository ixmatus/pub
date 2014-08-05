-----------------------------------------------------------------------------
-- |
-- Module      :  Pub.Internal
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
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pub.Internal where

import qualified Data.ByteString.Char8  as C8
import           Database.Redis
import           System.Console.CmdArgs
import           System.Log.Logger
import           Text.Groom             (groom)

-- | Data type for program CLI options.
data PArgs = PArgs
    { chan :: Maybe C8.ByteString
    , host :: Maybe String
    , port :: Maybe PortID
    , db   :: Maybe Int
    } deriving (Typeable, Show, Eq)

deriving instance Typeable PortID


-- | Data type for settings once merged from CLI.
data Settings = Settings
    { loglevel  :: !Priority
    , channel   :: Maybe C8.ByteString
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
                        , redisPort = port cliopts
                        , redisDB   = db cliopts
                        }

    debugM "Console" "Configuration parsed"
    debugM "Console" $ groom conf

    return conf
