module Hablog.Data.Page
( PageT
) where

import Control.Monad.Reader     (ReaderT)
import Hablog.Data.Sitemap      (Sitemap)
import Happstack.Server         (ServerPartT)
import Web.Routes               (RouteT)
import Web.Routes.Happstack     ()

type PageT state m = ReaderT state (RouteT Sitemap (ServerPartT m))

