module Hablog.Routes
( routes
) where

import Control.Monad.Reader     (runReaderT)
import Hablog.Data              (Page)
import Hablog.Data.RequestState (RequestState(..))
import Hablog.Data.Sitemap
import Happstack.Server         (Response, ServerPartT)
import Web.Routes               (RouteT)
import qualified Hablog.Pages       as Pages
import qualified Hablog.Admin.Pages as Admin

routes :: RequestState -> Sitemap -> RouteT Sitemap (ServerPartT IO) Response
routes rq url = runReaderT routes' rq
  where
    routes' :: Page Response
    routes' = case url of
      (Home)       -> Pages.home
      (Entry slug) -> Pages.entry slug
      (AdminHome)   -> Admin.home
      (AdminLogin)  -> Admin.login
      (AdminLogout) -> Admin.logout

