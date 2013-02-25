module Hablog(site) where

import Hablog.Data.Config (Config)
import Happstack.Server   (Response, ServerPartT, ok, toResponse)


site :: Config -> ServerPartT IO Response
site cfg = ok $ toResponse $ "Running with config:\n" ++ show cfg

