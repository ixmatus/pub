-----------------------------------------------------------------------------
-- |
-- Module      :  Pub.hs
-- Copyright   :  (C) 2014 Parnell Springmeyer
-- License     :  AllRightsReserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Module for defining the primary feeding pipes.
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Pub where

import           Control.Monad
import           Data.Maybe
import           Data.Text                 (Text, unpack)
import           Database.Redis
import           Filesystem.Path.CurrentOS as FS
import           Pipes
import qualified Pipes.ByteString          as PB
import           System.IO
import           System.Log.Logger
import           Text.Groom                (groom)

import           Pub.Internal

pipePublish :: Settings -> IO ()
pipePublish s = do
    cn <- connect conn
    runEffect $ PB.stdin >-> redisPub cn (channel s)
  where
    conn = defaultConnectInfo {
        connectHost     = fromMaybe
                            (connectHost defaultConnectInfo)
                            (redisHost s)
      , connectPort     = fromMaybe
                            (connectPort defaultConnectInfo)
                            (redisPort s)
      , connectDatabase = fromMaybe
                            (connectDatabase defaultConnectInfo)
                            (redisDB s)
    }

redisPub conn c = do
    inp <- await
    liftIO $ runRedis conn $ do
        publish c inp
