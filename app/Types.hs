{-# LANGUAGE DeriveGeneric #-}
module Types (ShortURL(..)) where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

data ShortURL = ShortURL {shortURL :: String} deriving (Show, Generic)

instance ToJSON ShortURL
instance FromJSON ShortURL
