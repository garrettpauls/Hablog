module Main(main) where

import Hablog             (site)
import Hablog.Data.Config
import Hablog.Data.Config.Database (runDatabase)
import Hablog.Data.Entry  (migrateAll)
import Happstack.Server   (Conf(port), nullConf, simpleHTTP)
import Database.Persist.GenericSql (runMigration)

main :: IO ()
main = do
  cfg <- loadConfigOrDefault "config.yaml"
  putStrLn $ show cfg
  runDatabase (cfgDatabase cfg) $ runMigration migrateAll
  simpleHTTP nullConf { port = cfgPort cfg } $ site cfg

