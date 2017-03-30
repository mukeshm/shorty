module Main where

import Routes (routes)
import Network.Wai.Middleware.RequestLogger
import Web.Scotty (scotty
                  , ScottyM
                  , middleware)
import System.Environment (getEnv)
import Database
import System.FilePath.Posix ((</>))

-- dev logger
devLogger :: ScottyM ()
devLogger = middleware logStdoutDev

getTemplatePath :: FilePath -> FilePath
getTemplatePath docRoot = docRoot </> "templates"

main :: IO ()
main = do
  docRoot <- getEnv "DOCUMENT_ROOT"
  port <- read <$> getEnv "SHORTY_PORT"
  redisHost <- getEnv "REDIS_HOST"
  redisPort <- getEnv "REDIS_PORT"
  rConn <- getDBConnection $ connectionInfo redisHost redisPort
  scotty port $ do
    devLogger
    routes rConn $ getTemplatePath docRoot
