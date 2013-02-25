module Main(main) where

import Hablog             (site)
import Hablog.Data.Config
import Happstack.Server   (Conf(port), nullConf, simpleHTTP)

main :: IO ()
main = do
  cfg <- loadConfigOrDefault "config.yaml"
  simpleHTTP nullConf { port = cfgPort cfg } $ site cfg

