{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty

main :: IO ()
main = scotty 8080 $ do
  get "/" $ do
    html "hello from scotty"
