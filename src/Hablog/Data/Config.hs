{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Hablog.Data.Config
( Config(..)
, nullConfig
, loadConfigOrDefault
) where

import Prelude hiding (catch)

import Control.Applicative   ((<$>), (<*>))
import Control.Exception     (SomeException, catch)
import Data.ByteString.Char8 (pack, unpack)
import Data.Maybe            (fromMaybe)
import Data.Text             (Text)
import Data.Yaml
import Hablog.Data.Config.Database

data Config =
     Config { cfgDomain  :: Text -- ^ The domain should not end with /, e.g. "http://localhost"
            , cfgPort    :: Int  -- ^ The port to listen on for connections
            , cfgAppRoot :: Text -- ^ The root path for the application, e.g. "/" or "/subapp"
            , cfgDatabase :: DBConfig -- ^ The connection settings for the database
            }

nullConfig :: Config
nullConfig = Config "http://localhost" 8000 "" nullDBConfig

instance ToJSON Config where
  toJSON cfg = object
    [ "domain"   .= cfgDomain cfg
    , "port"     .= cfgPort cfg
    , "appRoot"  .= cfgAppRoot cfg
    , "database" .= cfgDatabase cfg
    ]

instance FromJSON Config where
  parseJSON (Object v) = Config <$>
    v .: "domain"  <*>
    v .: "port"    <*>
    v .: "appRoot" <*>
    v .: "database"
  parseJSON _          = return nullConfig

instance Show Config where
  show = unpack . encode

loadConfigOrDefault :: FilePath -> IO Config
loadConfigOrDefault file = do
  content <- maybeReadFile file
  return $ fromMaybe nullConfig $ content >>= decodeStr
  where
    decodeStr :: FromJSON a => String -> Maybe a
    decodeStr = decode . pack
    maybeReadFile :: FilePath -> IO (Maybe String)
    maybeReadFile f = (readFile f >>= return . Just) `catch` (\(_ :: SomeException) -> return Nothing)

