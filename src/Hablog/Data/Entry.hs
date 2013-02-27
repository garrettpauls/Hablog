{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, GADTs, FlexibleContexts #-}
module Hablog.Data.Entry where

import Data.Text
import Database.Persist
import Database.Persist.TH
import Hablog.Data.Slug (Slug)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Entry
  slug  Slug
  title Text
  UniqueSlug slug
  deriving Show
|]

