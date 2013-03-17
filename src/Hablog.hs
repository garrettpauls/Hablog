{-# LANGUAGE OverloadedStrings #-}
module Hablog(site) where

import Prelude       hiding (concat)
import Control.Monad        (msum)
import Data.Text            (concat, pack)
import Database.Persist
import Hablog.Data.Config   (Config(..))
import Hablog.Data.Persist
import Hablog.Data.RequestState (RequestState(..))
import Hablog.Data.Session
import Hablog.Data.Sitemap  (Sitemap(..), sitemap)
import Hablog.Routes        (routes)
import Happstack.Server     (Response, ServerPartT, toResponse, dir, notFound, seeOther)
import Web.Routes           (Site, setDefault, runRouteT)
import Web.Routes.Boomerang (boomerangSite)
import Web.Routes.Happstack (implSite)

site :: Config -> ServerPartT IO Response
site cfg = msum
  [ dir "favicon.ico" $ notFound (toResponse ())
  , implSite'
  , seeOther (cfgAppRoot cfg) (toResponse ())
  ]
  where
    implSite' :: ServerPartT IO Response
    implSite' = do
      session <- maybeGetSession cfg
      implSite (concat [cfgDomain cfg, ":", pack $ show $ cfgPort cfg]) (cfgAppRoot cfg) $ site' session
    site' :: (Maybe (Entity Session)) -> Site Sitemap (ServerPartT IO Response)
    site' session = do
      let request = RequestState cfg session
      setDefault SiteHome $ boomerangSite (runRouteT $ routes request) sitemap

