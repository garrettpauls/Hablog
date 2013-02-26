{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell, TypeOperators #-}
module Hablog.Data.Sitemap
( Sitemap(..)
, EntryId(..)
, sitemap
) where

import Prelude    hiding ((.))
import Control.Category  (Category((.)))
import Data.Data         (Data, Typeable)
import Text.Boomerang.TH (derivePrinterParsers)
import Web.Routes        (PathInfo(..))
import Web.Routes.Boomerang

newtype EntryId = EntryId { unEntryId :: Int }
                  deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, PathInfo)

data Sitemap =
     Home
   | Entry EntryId
   deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePrinterParsers ''Sitemap)

sitemap :: Router () (Sitemap :- ())
sitemap =
  (  rHome
  <> rEntry . (lit "entry" </> entryId)
  )

entryId :: Router () (EntryId :- ())
entryId = xmaph EntryId (Just . unEntryId) int

