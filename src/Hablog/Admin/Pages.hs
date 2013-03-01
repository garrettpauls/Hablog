{-# LANGUAGE OverloadedStrings #-}
module Hablog.Admin.Pages
( home
, userHome
) where

import Prelude      hiding (head)
import Hablog.Data.Sitemap (Sitemap(..))
import Hablog.Pages        (Page)
import Happstack.Server    (ok, toResponse)
import Text.Blaze.Html5    ((!), Html, html, head, body, title, p, a, toValue)
import Text.Blaze.Html5.Attributes (href)
import Web.Routes          (showURL)

template :: Html -> Html -> Page
template h b = ok $ toResponse $ html $ do
  head h
  body b

home :: Page
home = do
  userHomeUrl <- showURL AdminUserHome
  template (title "Admin home") $ do
    p "Admin home"
    p $ a ! href (toValue userHomeUrl) $ "User admin"

userHome :: Page
userHome = template (title "User home") $ do
  p "User home"

