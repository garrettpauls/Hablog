{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, GADTs, FlexibleContexts #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module Hablog.Data where

import Control.Monad.Reader     (ReaderT, ask)
import Data.ByteString
import Data.Text
import Data.Time.Clock
import Database.Persist
import Database.Persist.TH
import Hablog.Data.Config       (Config)
import Hablog.Data.RequestState (RequestState(..))
import Hablog.Data.Sitemap      (Sitemap)
import Hablog.Data.Slug         (Slug)
import Happstack.Server         (ServerPartT)
import Web.Routes               (RouteT)

type Page = PageT IO
type PageT m = ReaderT RequestState (RouteT Sitemap (ServerPartT m))

getConfig :: Page Config
getConfig = ask >>= return . requestConfig

data MarkupEngine = Markdown deriving (Show, Read, Eq)
derivePersistField "MarkupEngine"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Category
  slug  Slug
  title Text
  UniqueCategory slug
  deriving Show

User
  userName    Text
  password    ByteString
  email       Text
  displayName Text
  UniqueUserName userName
  deriving Show

Entry
  slug         Slug
  title        Text
  authorId     UserId
  categoryId   Category Maybe
  markup       Text
  markupEngine MarkupEngine
  renderedHtml Text
  published    Bool
  publishDate  UTCTime
  updateDate   UTCTime
  UniqueEntry slug
  deriving Show
|]

