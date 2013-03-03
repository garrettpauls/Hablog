module Hablog.Data.RequestState
( RequestState(..)
, getConfig
, getSession
, isLoggedIn
, runDatabase
, whenLoggedIn
) where

import Control.Monad.Logger        (NoLoggingT)
import Control.Monad.Reader        (ask)
import Control.Monad.IO.Class      (liftIO)
import Data.Conduit                (ResourceT)
import Data.Maybe                  (isJust)
import Database.Persist            (Entity(..))
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

getSession :: PageT RequestState IO (Maybe (Entity Session))
getSession = ask >>= return . requestSession

runDatabase :: SqlPersist (NoLoggingT (ResourceT IO)) a -> PageT RequestState IO a
runDatabase f = do
  cfg <- getConfig
  liftIO $ DB.runDatabase (cfgDatabase cfg) f

isLoggedIn :: PageT RequestState IO Bool
isLoggedIn = ask >>= return . isJust . requestSession

whenLoggedIn :: PageT RequestState IO a -> PageT RequestState IO a -> PageT RequestState IO a
whenLoggedIn yes no = isLoggedIn >>= (\x -> if x then yes else no)

