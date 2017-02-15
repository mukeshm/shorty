{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Network.HTTP.Types (status301)
import System.Random (randomRIO)
import Control.Monad (replicateM)

-- list of unique chars
alphaNum :: String
alphaNum = ['a'..'z'] ++ ['0'..'9'] ++ ['A'..'Z']

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

-- dev logger
devLogger :: ScottyM ()
devLogger = middleware logStdoutDev

-- serve main page
serveMain :: ScottyM ()
serveMain = get "/" $ file "./templates/index.html"

-- redirect short codes
redirectURL :: ScottyM ()
redirectURL =  get "/:code" $ do
  -- get a url for short code if present
  -- else return 404
  status status301
  addHeader  "Location" "http://www.geekskool.com"

-- URL shortner api endpoint
shortenURL :: ScottyM ()
shortenURL = post "/url" $ do
    -- get form data
    -- get the url
    -- shorten and dump to db
    -- send back the short url
    html "Got shorten request"

-- handle all other routes
allOtherRoutes :: ScottyM ()
allOtherRoutes = notFound $ do
  html "Not Found"

app :: ScottyM ()
app = do
  devLogger
  serveMain
  redirectURL
  shortenURL
  allOtherRoutes

main :: IO ()
main = scotty 8080 app

