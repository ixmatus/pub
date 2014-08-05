-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (C) 2014 Parnell Springmeyer
-- License     :  AllRightsReserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- This is the entry point for the `pub` executable. Main sets up
-- the cli options configuration and parses configuration file options.
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Version           (showVersion)
import           Paths_pub              (version)
import           System.Console.CmdArgs

import           Pub
import           Pub.Internal

-- | Command line argument configuration.
--
-- Help messages, switches, options, and the human-readable version
-- number is configured here.
programArgs :: PArgs
programArgs = PArgs
    { chan  = def &= argPos 0 &= typ "CHANNEL"
    , host  = def &= name "h" &= typ "STRING" &= help "Redis host (default `localhost`)"
    , port  = def &= name "p" &= help "Redis port (default `6379`)"
    , db    = def &= name "d" &= help "Redis database (default `0`)"
    } &=
    verbosity &=
    help    "Pipe stdin to a pub/sub channel." &=
    summary ("pub v" ++ showVersion version) &=
    noAtExpand &=
    details ["Given an input on stdin, pipe to a redis pub/sub channel."]

main :: IO ()
main = cmdArgs programArgs >>= handleOpts >>= pipePublish
