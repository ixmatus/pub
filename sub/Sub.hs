-----------------------------------------------------------------------------
-- |
-- Module      :  Sub.hs
-- Copyright   :  (C) 2014 Parnell Springmeyer
-- License     :  AllRightsReserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Module for defining the primary feeding pipes.
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Sub where

import qualified Data.ByteString.Char8 as C8
import           Data.Maybe
import           Data.Monoid           (mempty)
import           Database.Redis

import           Sub.Internal

pipePublish :: Settings -> IO ()
pipePublish s = do
    cn <- connect conn
    redisSub cn (channel s)
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

redisSub :: Connection -> C8.ByteString -> IO ()
redisSub conn c = do
    runRedis conn $ do
        pubSub (subscribe [c]) $ \msg -> (C8.putStr $ msgMessage msg) >> return mempty

