{-# LANGUAGE OverloadedStrings #-}
module Hablog.Pages
( home
, entry
, Page
) where

import Prelude       hiding (head)
import Hablog.Data.Sitemap  (Sitemap(..))
import Hablog.Data.Slug
import Happstack.Server     (Response, ServerPartT, ok, toResponse)
import Text.Blaze.Html5     ((!), html, head, body, title, p, a, toHtml, toValue)
import Text.Blaze.Html5.Attributes (href)
import Web.Routes           (RouteT, showURL)
import Web.Routes.Happstack ()

type Page = RouteT Sitemap (ServerPartT IO) Response

home :: Page
home = do
  entryUrl <- showURL $ Entry (slugify "example entry")
  ok $ toResponse $ html $ do
    head $ title "Home"
    body $ do
      p "Home!"
      p $ a ! href (toValue entryUrl) $ "Example entry"

entry :: Slug -> Page
entry eid = do
  homeUrl <- showURL Home
  ok $ toResponse $ html $ do
    head $ title $ toHtml $ "Entry " ++ show eid
    body $ do
      p $ toHtml $ "Now viewing entry " ++ show eid
      p $ a ! href (toValue homeUrl) $ "Home"

