module Main where

import qualified Database.Redis as R
import Routes (routes)
import Network.Wai.Middleware.RequestLogger
import Web.Scotty (scotty
                  , ScottyM
                  , middleware)

-- dev logger
devLogger :: ScottyM ()
devLogger = middleware logStdoutDev

main :: IO ()
main = do
  -- TODO get port
  -- TODO get redis host,port, auth
  rConn <- R.checkedConnect R.defaultConnectInfo 
  scotty 8080 $ do
    devLogger
    routes rConn
