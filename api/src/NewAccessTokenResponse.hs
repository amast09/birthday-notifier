{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module NewAccessTokenResponse ( NewAccessTokenResponse, accessToken ) where

import GHC.Generics
import           Data.Aeson

data NewAccessTokenResponse = NewAccessTokenResponse {accessToken :: String
  , expiresIn :: Int
  , scope :: String
  , tokenType :: String} deriving (Eq, Generic, Show)

instance FromJSON NewAccessTokenResponse where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_', unwrapUnaryRecords = True}

instance ToJSON NewAccessTokenResponse
