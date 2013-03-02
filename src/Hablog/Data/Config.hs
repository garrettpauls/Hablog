{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Hablog.Data.Config
( Config(..)
, nullConfig
, loadConfigOrDefault
, decodeBodyCfg
) where

import Prelude         hiding (catch)

import Control.Exception      (SomeException, catch)
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Char8  (pack)
import Data.Maybe             (fromMaybe)
import Data.Yaml
import Hablog.Data.Config.Happstack
import Hablog.Data.Config.Internal
import Happstack.Server
import System.Directory       (getTemporaryDirectory)

loadConfigOrDefault :: FilePath -> IO Config
loadConfigOrDefault file = do
  content <- maybeReadFile file
  temp <- getTemporaryDirectory
  return $ fromMaybe (nullConfig temp) $ content >>= decodeStr
  where
    decodeStr :: FromJSON a => String -> Maybe a
    decodeStr = decode . pack
    maybeReadFile :: FilePath -> IO (Maybe String)
    maybeReadFile f = (readFile f >>= return . Just) `catch` (\(_ :: SomeException) -> return Nothing)

decodeBodyCfg :: (ServerMonad m, MonadPlus m, FilterMonad Response m, WebMonad Response m, MonadIO m)
              => Config -> m ()
decodeBodyCfg = decodeBody . makeBodyPolicy . cfgBodyPolicy

