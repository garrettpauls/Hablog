{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Hablog.Data.Config.Database
( DBConfig(..)
, nullDBConfig
, runDatabase
) where

import Control.Applicative         ((<$>))
import Control.Monad.IO.Class      (MonadIO(..))
import Control.Monad.Logger        (NoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.ByteString.Char8       (unpack)
import Data.Conduit                (ResourceT)
import Data.Text                   (Text)
import Data.Yaml
import Database.Persist.GenericSql (SqlPersist)
import Database.Persist.Sqlite     (runSqlite)

data DBConfig =
     DBSQLite { dbFile :: Text -- ^ The SQLite file
              }

nullDBConfig :: DBConfig
nullDBConfig = DBSQLite "hablog.sqlite"

instance ToJSON DBConfig where
  toJSON cfg = object
    [ "file" .= dbFile cfg
    ]

instance FromJSON DBConfig where
  parseJSON (Object v) = DBSQLite <$>
    v .: "file"
  parseJSON _          = return nullDBConfig

instance Show DBConfig where
  show = unpack . encode

runDatabase :: (MonadBaseControl IO m, MonadIO m)
            => DBConfig -> SqlPersist (NoLoggingT (ResourceT m)) a -> m a
runDatabase cfg f = case cfg of
  DBSQLite file -> runSqlite file f

