module Main where

import Routes (routes)
import Network.Wai.Middleware.RequestLogger
import Web.Scotty (scotty
                  , ScottyM
                  , middleware)
import System.Environment (getEnv)
import Database

-- dev logger
devLogger :: ScottyM ()
devLogger = middleware logStdoutDev

main :: IO ()
main = do
  port <- read <$> getEnv "SHORTY_PORT"
  redisHost <- getEnv "REDIS_HOST"
  redisPort <- getEnv "REDIS_PORT"
  rConn <- getDBConnection $ connectionInfo redisHost redisPort
  scotty port $ do
    devLogger
    routes rConn
