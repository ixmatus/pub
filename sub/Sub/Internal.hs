-----------------------------------------------------------------------------
-- |
-- Module      :  Sub.Internal
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

module Sub.Internal where

import           Control.Applicative
import qualified Data.ByteString.Char8   as C8
import           Database.Redis          as R
import           Network.Socket.Internal as NSI (PortNumber)
import           System.Console.CmdArgs
import           System.Log.Logger
import           Text.Groom              (groom)

-- | Data type for program CLI options.
data PArgs = PArgs
    { chan :: String
    , host :: Maybe String
    , port :: Maybe Integer
    , db   :: Maybe Integer
    } deriving (Data, Typeable, Show, Eq)

-- | Data type for settings once merged from CLI.
data Settings = Settings
    { loglevel  :: !Priority
    , channel   :: C8.ByteString
    , redisHost :: Maybe HostName
    , redisPort :: Maybe PortID
    , redisDB   :: Maybe Integer
    } deriving (Show, Eq)

-- | Given options, set the logging level, and create the settings
-- record.
handleOpts :: PArgs -> IO Settings
handleOpts cliopts = do
    whenNormal $ updateGlobalLogger "Console" (setLevel ERROR)
    whenLoud   $ updateGlobalLogger "Console" (setLevel DEBUG)

    let c    = C8.pack $ chan cliopts
        p    = (fromInteger <$> (port cliopts)) :: Maybe NSI.PortNumber
        conf = Settings { loglevel  = INFO
                        , channel   = c
                        , redisHost = host cliopts
                        , redisPort = R.PortNumber <$> p
                        , redisDB   = db cliopts
                        }

    debugM "Console" "Configuration parsed"
    debugM "Console" $ groom conf

    return conf
