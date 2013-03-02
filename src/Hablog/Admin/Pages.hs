{-# LANGUAGE OverloadedStrings #-}
module Hablog.Admin.Pages
( home
, login
, logout
) where

import Prelude           hiding (head)
import Hablog.Admin.Pages.Login (login)
import Hablog.Pages             (Page)
import Happstack.Server         (ok, toResponse)
import Text.Blaze.Html5         (Html, html, head, body, title, p)

template :: Html -> Html -> Page
template h b = ok $ toResponse $ html $ do
  head h
  body b

home :: Page
home = do
  template (title "Admin home") $ do
    p "Admin home"

logout :: Page
logout =
  template (title "Logout") $ do
    p "TODO: Logout"
