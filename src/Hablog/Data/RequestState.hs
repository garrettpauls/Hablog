module Hablog.Data.RequestState
( RequestState(..)
, getConfig
, runDatabase
) where

import Control.Monad.Logger        (NoLoggingT)
import Control.Monad.Reader        (ask)
import Control.Monad.IO.Class      (liftIO)
import Data.Conduit                (ResourceT)
import Database.Persist            (Entity)
import Database.Persist.GenericSql (SqlPersist)
import Hablog.Data.Config          (Config(..))
import Hablog.Data.Page
import Hablog.Data.Persist
import qualified Hablog.Data.Config.Database as DB

data RequestState = RequestState
  { requestConfig  :: Config
  , requestSession :: Maybe (Entity Session)
  } deriving Show

getConfig :: PageT RequestState IO Config
getConfig = ask >>= return . requestConfig

runDatabase :: SqlPersist (NoLoggingT (ResourceT IO)) a -> PageT RequestState IO a
runDatabase f = do
  cfg <- getConfig
  liftIO $ DB.runDatabase (cfgDatabase cfg) f

