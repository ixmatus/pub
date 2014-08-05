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

import           Data.Maybe
import           Database.Redis
import           Pipes
import qualified Pipes.ByteString as PB

import           Pub.Internal

pipePublish :: Settings -> IO ()
pipePublish s = do
    cn <- connect conn
    runEffect $ PB.stdin >-> redisPub cn (channel s) >-> showResult
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

showResult :: Consumer (Either Reply Integer) IO ()
showResult = await >>= (liftIO . print . show)

redisPub :: Connection -> PB.ByteString -> Pipe PB.ByteString (Either Reply Integer) IO ()
redisPub conn c = do
    inp <- await
    r   <- liftIO $ runRedis conn $ do
        publish c inp
    yield r
