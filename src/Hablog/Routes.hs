module Hablog.Routes
( routes
) where

import Hablog.Data.Sitemap (Sitemap(..))
import Happstack.Server    (Response, ServerPartT)
import Web.Routes          (RouteT)
import qualified Hablog.Pages as Page

routes :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
routes url = case url of
  (Home)      -> Page.home
  (Entry eId) -> Page.entry eId

