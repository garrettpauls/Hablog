{-# LANGUAGE OverloadedStrings #-}
module Hablog.Admin.Pages
( home
, login
, logout
) where

import Prelude            hiding (head)
import Control.Monad.Trans.Class (lift)
import Hablog.Admin.Pages.Login  (login)
import Hablog.Data               (Page)
import Hablog.Data.Session
import Hablog.Data.Sitemap       (Sitemap(..))
import Hablog.Data.RequestState
import Happstack.Server          (Response, ok, toResponse, tempRedirect)
import Text.Blaze.Html5          (Html, html, head, body, title, p)
import Web.Routes                (showURL)
import qualified Data.Text        as T

template :: Html -> Html -> Page Response
template h b = ok $ toResponse $ html $ do
  head h
  body b

home :: Page Response
home = do
  template (title "Admin home") $ do
    p "Admin home"

logout :: Page Response
logout = do
  session <- getSession
  maybe (return ()) (destorySession) session
  loginUrl <- lift $ showURL AdminLogin >>= return . T.unpack
  tempRedirect loginUrl $ toResponse ("You have been logged out." :: String)

