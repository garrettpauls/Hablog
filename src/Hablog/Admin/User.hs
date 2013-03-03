{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Hablog.Admin.User
( createUser
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.BCrypt
import Data.Text
import Data.Text.Encoding     (encodeUtf8)
import Database.Persist
import Hablog.Data.Persist

-- | Creates a user
createUser :: (PersistMonadBackend m ~ PersistEntityBackend User, PersistStore m)
           => Text -- ^ username
           -> Text -- ^ password
           -> Text -- ^ email
           -> Text -- ^ display name
           -> m (Either (Key User) Text)
createUser username password email displayName = do
  mHashedPass <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (encodeUtf8 password)
  case mHashedPass of
    Nothing         -> return $ Right "Failed to hash password"
    Just hashedPass -> do
      key <- insert (User username hashedPass email displayName)
      return $ Left key

