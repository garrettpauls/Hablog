{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, GADTs, FlexibleContexts #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module Hablog.Data.Persist where

import Data.ByteString
import Data.Text
import Data.Time.Clock
import Database.Persist
import Database.Persist.TH
import Hablog.Data.Markup  (MarkupEngine)
import Hablog.Data.Slug    (Slug)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Category
  slug  Slug
  title Text
  UniqueCategory slug
  deriving Show

User
  name        Text
  password    ByteString
  email       Text
  displayName Text
  UniqueUserName name
  deriving Show

Entry
  slug         Slug
  title        Text
  authorId     UserId
  categoryId   Category Maybe
  markup       Text
  markupEngine MarkupEngine
  html         Text
  published    Bool
  publishDate  UTCTime
  updateDate   UTCTime
  UniqueEntry slug
  deriving Show

Session
  key    ByteString
  userId UserId
  UniqueSession key
  deriving Show
|]

