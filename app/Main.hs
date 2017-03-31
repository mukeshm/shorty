module Main where

import Routes (routes)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Web.Scotty (scotty
                  , ScottyM
                  , middleware)
import System.Environment (getEnv)
import Database
import System.FilePath.Posix ((</>))

-- dev logger
devLogger :: ScottyM ()
devLogger = middleware logStdoutDev

staticMiddleware ::FilePath -> ScottyM()
staticMiddleware path = middleware $ staticPolicy $ addBase path 

getTemplatePath :: FilePath -> FilePath
getTemplatePath docRoot = docRoot </> "templates"

getStaticPath :: FilePath -> FilePath
getStaticPath docRoot = docRoot </> "static"

main :: IO ()
main = do
  docRoot <- getEnv "DOCUMENT_ROOT"
  port <- read <$> getEnv "SHORTY_PORT"
  redisHost <- getEnv "REDIS_HOST"
  redisPort <- getEnv "REDIS_PORT"
  rConn <- getDBConnection $ connectionInfo redisHost redisPort
  scotty port $ do
    devLogger
    staticMiddleware $ getStaticPath docRoot
    routes rConn $ getTemplatePath docRoot
