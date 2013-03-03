{-# LANGUAGE OverloadedStrings #-}
module Hablog(site) where

import Prelude       hiding (concat)
import Control.Monad        (msum)
import Data.Text            (concat, pack)
import Hablog.Data.Config   (Config(..))
import Hablog.Data.RequestState (RequestState(..))
import Hablog.Data.Sitemap  (Sitemap(..), sitemap)
import Hablog.Routes        (routes)
import Happstack.Server     (Response, ServerPartT, toResponse, dir, notFound, seeOther)
import Web.Routes           (Site, setDefault, runRouteT)
import Web.Routes.Boomerang (boomerangSite)
import Web.Routes.Happstack (implSite)

site :: Config -> ServerPartT IO Response
site cfg = msum
  [ dir "favicon.ico" $ notFound (toResponse ())
  , implSite (concat [cfgDomain cfg, ":", pack $ show $ cfgPort cfg]) (cfgAppRoot cfg) site'
  , seeOther (cfgAppRoot cfg) (toResponse ())
  ]
  where
    site' :: Site Sitemap (ServerPartT IO Response)
    site' = do
      let request = RequestState cfg
      setDefault Home $ boomerangSite (runRouteT $ routes request) sitemap

