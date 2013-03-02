{-# LANGUAGE OverloadedStrings #-}
module Hablog.Data.Config.Happstack
( ConfigBodyPolicy(..)
, nullConfigBodyPolicy
, makeBodyPolicy
) where

import Control.Applicative   ((<$>), (<*>))
import Data.ByteString.Char8 (unpack)
import Data.Int
import Data.Yaml
import Happstack.Server

data ConfigBodyPolicy = ConfigBodyPolicy
  { cfgTemp      :: FilePath
  , cfgMaxDisk   :: Int64
  , cfgMaxRam    :: Int64
  , cfgMaxHeader :: Int64
  }

instance ToJSON ConfigBodyPolicy where
  toJSON cfg = object
    [ "temp"      .= cfgTemp      cfg
    , "maxDisk"   .= cfgMaxDisk   cfg
    , "maxRam"    .= cfgMaxRam    cfg
    , "maxHeader" .= cfgMaxHeader cfg
    ]

instance FromJSON ConfigBodyPolicy where
  parseJSON (Object v) = ConfigBodyPolicy <$>
    v .: "temp"    <*>
    v .: "maxDisk" <*>
    v .: "maxRam"  <*>
    v .: "maxHeader"
  parseJSON _          = return $ nullConfigBodyPolicy "/tmp/"

instance Show ConfigBodyPolicy where
  show = unpack . encode

nullConfigBodyPolicy :: FilePath -> ConfigBodyPolicy
nullConfigBodyPolicy temp = ConfigBodyPolicy temp 4096 4096 4096

makeBodyPolicy :: ConfigBodyPolicy -> BodyPolicy
makeBodyPolicy cfg = defaultBodyPolicy (cfgTemp cfg) (cfgMaxDisk cfg) (cfgMaxRam cfg) (cfgMaxHeader cfg)

