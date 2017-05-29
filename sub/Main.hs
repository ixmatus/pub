-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (C) 2014 Parnell Springmeyer
-- License     :  AllRightsReserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import           Control.Monad
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Maybe
import           Data.Version          (showVersion)
import qualified Database.Redis        as Redis
import           Options.Generic
import qualified Paths_pub             as Pub
import qualified System.IO

data Options w = Options
  { channel :: w ::: [ByteString]  <?> "Redis channel(s) to subscribe to"
  , host    :: w ::: Maybe String  <?> "Redis host (default: localhost)"
  , port    :: w ::: Maybe Integer <?> "Redis post (default: 6379)"
  , db      :: w ::: Maybe Integer <?> "Redis db   (default: 0)"
  , version :: Bool
  } deriving (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

main :: IO ()
main = do
  Options{..} <- unwrapRecord "Subscribe to redis channels and pipe to stdout"
  let stderr = System.IO.stderr
  let stdout = System.IO.stdout
  when version $
    System.IO.hPutStrLn stderr (showVersion Pub.version)
  conn <-
    Redis.connect
      Redis.defaultConnectInfo
       { Redis.connectHost     = (fromMaybe "localhost" host)
       , Redis.connectPort     = Redis.PortNumber (fromInteger $ fromMaybe 6379 port)
       , Redis.connectDatabase = fromMaybe 0 db
       }

  Redis.runRedis conn $
    Redis.pubSub
     (Redis.subscribe channel)
     (\msg -> do
         C8.putStrLn $ Redis.msgMessage msg
         System.IO.hFlush stdout
         pure mempty)
