module Database (
  getURL
  , saveURL
  , getDBConnection
  , connectionInfo
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Database.Redis as R

getDBConnection :: R.ConnectInfo -> IO R.Connection
getDBConnection ci = R.checkedConnect ci

connectionInfo :: String -> String -> R.ConnectInfo
connectionInfo host port = R.defaultConnectInfo
                           { R.connectHost = host
                           , R.connectPort = R.Service port}

getURL  :: R.Connection
        -> BS.ByteString
        -> IO (Either R.Reply (Maybe BS.ByteString))
getURL conn shortCode = R.runRedis conn $ R.get shortCode

saveURL :: R.Connection
        -> BS.ByteString
        -> BS.ByteString
        -> IO (Either R.Reply R.Status)
saveURL conn shortCode url = R.runRedis conn $ R.set shortCode url

