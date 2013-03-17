module Hablog.Admin.Pages
( home
, login
, logout
, entryNew
, entryList
) where

import Prelude            hiding (head)
import Control.Monad.Trans.Class (lift)
import Hablog.Admin.Pages.Login  (login)
import Hablog.Admin.Pages.Entry  (entryNew, entryList)
import Hablog.Data               (Page)
import Hablog.Data.Session
import Hablog.Data.Sitemap       (Sitemap(..))
import Hablog.Data.RequestState
import Hablog.Util
import Happstack.Server          (Response, ok, toResponse, tempRedirect)
import Text.Blaze.Html5          (Html, (!), html, toHtml, toValue)
import Web.Routes                (showURL)
import qualified Data.Text        as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

template :: Html -> Html -> Page Response
template h b = ok $ toResponse $ html $ do
  H.head h
  H.body b

home :: Page Response
home = do
  urls <- lift $ sequence
    [ showURL AdminEntryNew  >>= return . mkTwo "New entry"
    , showURL AdminEntryList >>= return . mkTwo "List entries"
    , showURL AdminLogout    >>= return . mkTwo "Logout"
    ]
  template (H.title $ toHtml "Admin home") $ do
    H.p $ toHtml "Admin home"
    H.hr
    H.ul $ mapM_ (\(lbl, href) -> H.li $ H.a ! A.href (toValue href) $ toHtml lbl ) urls

logout :: Page Response
logout = do
  session <- getSession
  maybe (return ()) (destorySession) session
  loginUrl <- lift $ showURL SiteHome >>= return . T.unpack
  tempRedirect loginUrl $ toResponse "You have been logged out."

