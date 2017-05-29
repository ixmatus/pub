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
import           Data.ByteString  (ByteString)
import           Data.Maybe
import           Data.Version     (showVersion)
import qualified Database.Redis   as Redis
import           Options.Generic
import qualified Paths_pub        as Pub
import           Pipes
import qualified Pipes.ByteString
import qualified System.IO

data Options w = Options
  { channel :: w ::: ByteString    <?> "Redis channel to publish to"
  , host    :: w ::: Maybe String  <?> "Redis host (default: localhost)"
  , port    :: w ::: Maybe Integer <?> "Redis post (default: 6379)"
  , db      :: w ::: Maybe Integer <?> "Redis db   (default: 0)"
  , version :: Bool
  } deriving (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

main :: IO ()
main = do
  Options{..} <- unwrapRecord "Pipe stdin to a redis pub/sub channel"
  let stderr = System.IO.stderr
  when version $
    System.IO.hPutStrLn stderr (showVersion Pub.version)
  conn <-
    Redis.connect
      Redis.defaultConnectInfo
       { Redis.connectHost     = (fromMaybe "localhost" host)
       , Redis.connectPort     = Redis.PortNumber (fromInteger $ fromMaybe 6379 port)
       , Redis.connectDatabase = fromMaybe 0 db
       }

  let publish =
        (\value ->
          -- TODO: should we use runRedis here!?!?
          lift (Redis.runRedis conn $ Redis.publish channel value) >>= \case
            Left reply -> lift (System.IO.hPutStrLn stderr $ show reply)
            Right _    -> pure ())

  runEffect (for Pipes.ByteString.stdin publish)
