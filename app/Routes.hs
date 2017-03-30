{-# LANGUAGE OverloadedStrings #-}

module Routes (routes) where

import Network.HTTP.Types (status301, status404, status400)
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Network.URI (URI, parseURI, uriToString, parseRelativeReference, relativeTo)
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as BS
import Control.Monad.IO.Class (liftIO)
import qualified Database.Redis as R
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database (getURL, saveURL)
import System.FilePath.Posix ((</>))
import Types (ShortURL(..)
             , ShortyError(..))
import Web.Scotty (ScottyM
                  , html, notFound
                  , status, param
                  , post, get
                  , addHeader, text
                  , file, json)

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
        html "Invalid URL : Not Found"
      Just url -> do
        status status301
        addHeader  "Location" (bsToText url)

createShortUrl :: String -> String -> Maybe String
createShortUrl d s = let sc = parseRelativeReference s
                         dom = parseURI d
                     in (\x -> uriToString id x "") <$> (relativeTo <$> sc <*> dom)

-- URL shortner api endpoint
shortenURL :: R.Connection -> ScottyM ()
shortenURL conn = post "/url" $ do
    uri <- param "uri"
    domain <- param "domain"
    let parsedURI :: Maybe URI
        parsedURI = parseURI (TL.unpack uri)
    case parsedURI of
      Just _ -> do
        shortCode <- liftIO generateCode
        let shorty = BS.pack shortCode
            uri' = encodeUtf8 (TL.toStrict uri)
        resp <- liftIO (saveURL conn shorty uri')
        case resp of
          Left reply -> do
            status status400
            json $ ShortyError (show reply)
          Right stat -> case createShortUrl domain shortCode of
            Nothing -> do
              status status400
              json $ ShortyError "Invalid domain name"
            Just url -> do
              json $ ShortURL url
      Nothing -> do
        status status400
        json $ ShortyError "Invalid URL"

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
