module Main where

import Routes (routes)
import Network.Wai.Middleware.RequestLogger
import Web.Scotty (scotty
                  , ScottyM
                  , middleware)
import Database

-- dev logger
devLogger :: ScottyM ()
devLogger = middleware logStdoutDev

main :: IO ()
main = do
  -- TODO get port
  -- TODO get redis host,port, auth
  rConn <- getDBConnection connectionInfo
  scotty 8080 $ do
    devLogger
    routes rConn
