{-# LANGUAGE OverloadedStrings #-}
module Hablog.Pages
( home
, entry
) where

import Prelude       hiding (head)
import Control.Monad.Trans.Class (lift)
import Hablog.Data          (Page)
import Hablog.Data.Sitemap  (Sitemap(..))
import Hablog.Data.Slug
import Happstack.Server     (Response, ok, toResponse)
import Text.Blaze.Html5     ((!), html, head, body, title, p, a, toHtml, toValue)
import Text.Blaze.Html5.Attributes (href)
import Web.Routes           (showURL)
import Web.Routes.Happstack ()

home :: Page Response
home = do
  entryUrl <- lift $ showURL $ Entry (slugify "example entry")
  ok $ toResponse $ html $ do
    head $ title "Home"
    body $ do
      p "Home!"
      p $ a ! href (toValue entryUrl) $ "Example entry"

entry :: Slug -> Page Response
entry eid = do
  homeUrl <- lift $ showURL Home
  ok $ toResponse $ html $ do
    head $ title $ toHtml $ "Entry " ++ show eid
    body $ do
      p $ toHtml $ "Now viewing entry " ++ show eid
      p $ a ! href (toValue homeUrl) $ "Home"

