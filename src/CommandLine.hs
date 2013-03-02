module CommandLine(runCommandLine) where

import Control.Exception  (bracket_)
import Data.Text          (pack)
import Database.Persist.GenericSql (runMigration)
import Hablog.Admin.User
import Hablog.Data        (migrateAll)
import Hablog.Data.Config
import Hablog.Data.Config.Database (runDatabase)
import System.IO

runCommandLine :: Config -> [String] -> IO ()
runCommandLine cfg args = case args of
  ("migrate":_)     -> runDatabase (cfgDatabase cfg) $ runMigration migrateAll
  ("user":"add":_) -> addUser cfg
  _ -> putStrLn "Unknown arguments"

addUser :: Config -> IO ()
addUser cfg = do
  (name:pass:confPass:email:displayName:[]) <- runPrompts
    [ ("Username: ", True)
    , ("Password: ", False), ("Confirm password: ", False)
    , ("Email: ", True)
    , ("Display name: ", True) ]
  if pass /= confPass
    then putStrLn "The password and confirmation did not match."
    else do
      result <- runDatabase (cfgDatabase cfg) $ createUser (pack name) (pack pass) (pack email) (pack displayName)
      case result of
        Left  _   -> putStrLn "User created successfully"
        Right err -> putStrLn $ "Failed to create user: " ++ show err

runPrompts :: [(String, Bool)] -> IO [String]
runPrompts [] = return []
runPrompts ((prompt, echo):xs) = do
  putStr prompt
  hFlush stdout
  result <- withEcho echo getLine
  if not echo then putStrLn "" else return ()
  rest <- runPrompts xs
  return $ result:rest

withEcho :: Bool -> IO a -> IO a
withEcho echo f = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) f

