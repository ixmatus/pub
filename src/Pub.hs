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
import           Control.Monad.State.Strict
import           Data.Attoparsec.Text
import           Data.Text                  (Text, unpack)
import           Database.Redis
import           Filesystem.Path.CurrentOS  as FS
import           Pipes
import qualified Pipes.Prelude              as PP
import           System.IO
import           System.Log.Logger
import           Text.Groom                 (groom)

import           Pub.Internal

pipePublish :: Settings -> IO ()
pipePublish s = do
    cn <- connect conn
    runEffect $ PP.stdin >-> toChannel
  where
    conn      = defaultConnectInfo
    toChannel = redisPub

redisPub conn c = do
    inp <- await
    runRedis conn $ do
        publish c inp


parsingPipe :: Monad m => Producer Text m () -> m ParseResult
parsingPipe i = evalStateT (PA.parse (many1 seriesBlockParser)) i

unwrapParse :: ParseResult -> IO ()
unwrapParse Nothing  = print ("Nothing here..." :: String)
unwrapParse (Just v) = mapM_ fmt out
  where
    fmt s = do
      debugM "Console" $ (groom s ++ "\n")
      putStrLn $ (unpack $ seriesName s) ++ ": " ++ (show $ seriesSQC s)
    out   = case v of
              Left _  -> []
              Right s -> fmap statQuality s
