module Main(main) where

import CommandLine
import Hablog             (site)
import Hablog.Data.Config
import Happstack.Server   (Conf(port), nullConf, simpleHTTP)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  cfg <- loadConfigOrDefault "config.yaml"
  putStrLn $ show cfg
  if length args == 0 then runServer cfg else runCommandLine cfg args

runServer :: Config -> IO ()
runServer cfg = do
  simpleHTTP nullConf { port = cfgPort cfg } $ site cfg

