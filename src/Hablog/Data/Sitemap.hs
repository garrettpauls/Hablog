{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell, TypeOperators #-}
module Hablog.Data.Sitemap
( Sitemap(..)
, sitemap
) where

import Prelude    hiding ((.))
import Control.Category  (Category((.)))
import Data.Data         (Data, Typeable)
import Data.Text
import Hablog.Data.Slug
import Text.Boomerang.TH (derivePrinterParsers)
import Web.Routes.Boomerang

data Sitemap =
     Home
   | Entry Slug
   | AdminHome
   | AdminUserHome
   deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePrinterParsers ''Sitemap)

sitemap :: Router () (Sitemap :- ())
sitemap =
  (  rHome
  <> rEntry . (lit "entry" </> slug)
  <> lit "admin" . admin
  )
  where
    admin =  rAdminHome
          <> rAdminUserHome </> lit "user"

slug :: Router () (Slug :- ())
slug = xmaph (slugify . unpack) (Just . pack . show) anyText

