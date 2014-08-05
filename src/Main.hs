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
import           Paths_pub_parse        (version)
import           System.Console.CmdArgs

import           Pub
import           Pub.Internal

-- | Command line argument configuration.
--
-- Help messages, switches, options, and the human-readable version
-- number is configured here.
programArgs :: PArgs
programArgs = PArgs
    { chan  = def &= help "Channel to publish on"
    , host  = def &= help "Redis host"
    , port  = def &= help "Redis port"
    , db    = def &= help "Redis database (default 0)"
    } &=
    verbosity &=
    help    "Pipe stdin to a pub/sub channel." &=
    helpArg [name "h"] &=
    summary ("pub v" ++ showVersion version) &=
    noAtExpand &=
    details ["Given an input on stdin, pipe to a redis pub/sub channel."]

main :: IO ()
main = cmdArgs programArgs >>= handleOpts >>= pipePublish
