{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Hablog.Admin.User
( createUser
, authenticateUser
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.BCrypt
import Data.ByteString        (ByteString)
import Data.Text
import Data.Text.Encoding     (encodeUtf8)
import Database.Persist
import Hablog.Data
import Hablog.Data.Persist

-- | Creates a user
createUser :: (PersistMonadBackend m ~ PersistEntityBackend User, PersistStore m)
           => Text -- ^ username
           -> Text -- ^ password
           -> Text -- ^ email
           -> Text -- ^ display name
           -> m (Either (Key User) Text)
createUser username password email displayName = do
  mHashedPass <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (encodePass password)
  case mHashedPass of
    Nothing         -> return $ Right "Failed to hash password"
    Just hashedPass -> do
      key <- insert (User username hashedPass email displayName)
      return $ Left key

encodePass :: Text -> ByteString
encodePass = encodeUtf8

authenticateUser :: Text -> Text -> Page (Maybe (Entity User))
authenticateUser username password = runDatabase $ do
  user <- selectFirst [UserName ==. username] []
  let isValid = maybe False (checkUserPassword password) user
  return $ if isValid then user else Nothing

checkUserPassword :: Text -> (Entity User) -> Bool
checkUserPassword password (Entity _ user) =
  validatePassword (userPassword user) (encodePass password)

