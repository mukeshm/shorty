module Database (getURL, saveURL) where

import qualified Data.ByteString.Char8 as BS
import qualified Database.Redis as R

getURL  :: R.Connection
        -> BS.ByteString
        -> IO (Either R.Reply (Maybe BS.ByteString))
getURL conn shortCode = R.runRedis conn $ R.get shortCode

saveURL :: R.Connection
        -> BS.ByteString
        -> BS.ByteString
        -> IO (Either R.Reply R.Status)
saveURL conn shortCode url = R.runRedis conn $ R.set shortCode url

