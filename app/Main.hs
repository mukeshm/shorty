{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Data.Monoid ((<>))

-- dev logger
devLogger :: ScottyM ()
devLogger = middleware logStdoutDev

-- serve main page
serveMain :: ScottyM ()
serveMain = get "/" $ do
    html "hello from scotty"

-- redirect short codes
redirectURL :: ScottyM ()
redirectURL =  get "/:code" $ do
    code <- param "code"
    html $ "got short code : " <> code

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

