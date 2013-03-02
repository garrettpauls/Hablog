module Hablog.Routes
( routes
) where

import Hablog.Data.Config
import Hablog.Data.Sitemap
import Happstack.Server (Response, ServerPartT)
import Web.Routes       (RouteT)
import qualified Hablog.Pages as Page
import qualified Hablog.Admin.Pages as Admin

routes :: Config -> Sitemap -> RouteT Sitemap (ServerPartT IO) Response
routes cfg url = case url of
  (Home)       -> Page.home
  (Entry slug) -> Page.entry slug
  (AdminHome)   -> Admin.home
  (AdminLogin)  -> Admin.login cfg
  (AdminLogout) -> Admin.logout

