{-# LANGUAGE DeriveGeneric #-}
module Types (ShortURL(..)
             , ShortyError(..)) where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

data ShortURL = ShortURL {shortURL :: String} deriving (Show, Generic)

instance ToJSON ShortURL
instance FromJSON ShortURL

data ShortyError = ShortyError {errorText :: String} deriving (Show, Generic)

instance ToJSON ShortyError
instance FromJSON ShortyError
