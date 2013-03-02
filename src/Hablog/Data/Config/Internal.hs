{-# LANGUAGE OverloadedStrings #-}
module Hablog.Data.Config.Internal
( Config(..)
, nullConfig
) where

import Control.Applicative   ((<$>), (<*>))
import Data.ByteString.Char8 (unpack)
import Data.Text             (Text)
import Data.Yaml
import Hablog.Data.Config.Database
import Hablog.Data.Config.Happstack

data Config =
     Config { cfgDomain  :: Text -- ^ The domain should not end with /, e.g. "http://localhost"
            , cfgPort    :: Int  -- ^ The port to listen on for connections
            , cfgAppRoot :: Text -- ^ The root path for the application, e.g. "/" or "/subapp"
            , cfgDatabase :: DBConfig -- ^ The connection settings for the database
            , cfgBodyPolicy :: ConfigBodyPolicy -- ^ The body policy for the server
            }

nullConfig :: FilePath -> Config
nullConfig temp = Config "http://localhost" 8000 "" nullDBConfig (nullConfigBodyPolicy temp)

instance ToJSON Config where
  toJSON cfg = object
    [ "domain"     .= cfgDomain     cfg
    , "port"       .= cfgPort       cfg
    , "appRoot"    .= cfgAppRoot    cfg
    , "database"   .= cfgDatabase   cfg
    , "bodyPolicy" .= cfgBodyPolicy cfg
    ]

instance FromJSON Config where
  parseJSON (Object v) = Config <$>
    v .: "domain"   <*>
    v .: "port"     <*>
    v .: "appRoot"  <*>
    v .: "database" <*>
    v .: "bodyPolicy"
  parseJSON _          = return $ nullConfig ""

instance Show Config where
  show = unpack . encode

