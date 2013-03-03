{-# LANGUAGE FlexibleContexts #-}
module Hablog.Data.Session
( createSession
, maybeGetSession
, destorySession
) where

import Control.Monad.IO.Class   (liftIO)
import Data.ByteString.Base64   (encode, decode)
import Data.Char                (toLower)
import Database.Persist
import Hablog.Data.Config       (Config(..))
import Hablog.Data.Page         (PageT)
import Hablog.Data.Persist
import Hablog.Data.RequestState (RequestState, runDatabase)
import Happstack.Server  hiding (Session)
import qualified Data.ByteString.Char8       as BC
import qualified Hablog.Data.Config.Database as DB
import qualified Happstack.Server            as S

createSession :: Key User -> PageT RequestState IO Session
createSession userId = do
  let session = Session (BC.pack "TODO: make session id") (userId)
  _ <- runDatabase $ insert session
  addCookie S.Session $ mkCookie "session" (BC.unpack $ encode $ sessionKey session)
  return session

maybeGetSession :: Config -> ServerPartT IO (Maybe (Entity Session))
maybeGetSession cfg = do
  maybeSessionId <- maybeLookCookieValue "session" >>= return . (fmap (decode . BC.pack))
  case maybeSessionId of
    Just (Right key) -> liftIO $ DB.runDatabase (cfgDatabase cfg) $ getBy $ UniqueSession key
    _                -> return Nothing

destorySession :: (Entity Session) -> PageT RequestState IO ()
destorySession session = do
  runDatabase $ delete $ entityKey session
  expireCookie "session"

maybeLookCookie :: (Monad m, HasRqData m) => String -> m (Maybe Cookie)
maybeLookCookie name = do
  (_, _, cookies) <- askRqEnv
  return $ lookup (map toLower name) cookies

maybeLookCookieValue :: (Monad m, HasRqData m) => String -> m (Maybe String)
maybeLookCookieValue name = do
  c <- maybeLookCookie name
  return $ fmap cookieValue c

