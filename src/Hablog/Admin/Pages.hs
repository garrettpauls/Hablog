{-# LANGUAGE OverloadedStrings #-}
module Hablog.Admin.Pages
( home
, login
, logout
) where

import Prelude           hiding (head)
import Hablog.Admin.Pages.Login (login)
import Hablog.Data              (Page)
import Happstack.Server         (Response, ok, toResponse)
import Text.Blaze.Html5         (Html, html, head, body, title, p)

template :: Html -> Html -> Page Response
template h b = ok $ toResponse $ html $ do
  head h
  body b

home :: Page Response
home = do
  template (title "Admin home") $ do
    p "Admin home"

logout :: Page Response
logout =
  template (title "Logout") $ do
    p "TODO: Logout"
