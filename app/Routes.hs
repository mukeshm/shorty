{-# LANGUAGE OverloadedStrings #-}

module Routes (routes) where

import Network.HTTP.Types (status301, status404)
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Network.URI (URI, parseURI)
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as BS
import Control.Monad.IO.Class (liftIO)
import qualified Database.Redis as R
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database (getURL, saveURL)
import System.FilePath.Posix ((</>))
import Web.Scotty (ScottyM
                  , html
                  , notFound
                  , status
                  , param
                  , post
                  , get
                  , addHeader
                  , text
                  , file)

-- list of unique chars
alphaNum :: String
alphaNum = ['a'..'z'] ++ ['0'..'9'] ++ ['A'..'Z']

bsToText :: BS.ByteString -> TL.Text
bsToText bs = TL.fromStrict (decodeUtf8 bs)

-- selects random char from a list of chars
randomChar :: String -> IO Char
randomChar xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  randomDigit <- randomRIO (0, maxIndex) :: IO Int
  return (xs !! randomDigit)

-- generate 7 digit random code
generateCode :: IO String
generateCode = replicateM 7 (randomChar alphaNum)

-- serve main page
serveMain :: FilePath -> ScottyM ()
serveMain tPath = get "/" $ file (tPath </> "index.html")

-- redirect short codes
redirectURL :: R.Connection -> ScottyM ()
redirectURL conn =  get "/:code" $ do
  code <- param "code"
  eitherURL <- liftIO (getURL conn code)
  case eitherURL of
    Left reply -> do
      text (TL.pack (show reply))
    Right maybeURL -> case maybeURL of
      Nothing -> do
        status status404
        html "Invalid URL"
      Just url -> do
        status status301
        addHeader  "Location" (bsToText url)

-- URL shortner api endpoint
shortenURL :: R.Connection -> ScottyM ()
shortenURL conn = post "/url" $ do
    uri <- param "uri"
    let parsedURI :: Maybe URI
        parsedURI = parseURI (TL.unpack uri)
    case parsedURI of
      Just _ -> do
        shortCode <- liftIO generateCode
        let shorty = BS.pack shortCode
            uri' = encodeUtf8 (TL.toStrict uri)
        resp <- liftIO (saveURL conn shorty uri')
        case resp of
          Left reply -> text (TL.pack (show reply))
          Right status -> html $ TL.pack shortCode
      Nothing -> do
        status status404
        html "Invalid URL"

-- handle all other routes
allOtherRoutes :: ScottyM ()
allOtherRoutes = notFound $ do
  html "Not Found"

routes :: R.Connection -> String -> ScottyM ()
routes conn templatePath = do
  serveMain templatePath
  redirectURL conn
  shortenURL conn
  allOtherRoutes
