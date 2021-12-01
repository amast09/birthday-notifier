{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module NewAccessTokenResponse (NewAccessTokenResponse, accessToken) where

import Data.Aeson
import GHC.Generics

data NewAccessTokenResponse = NewAccessTokenResponse
  { accessToken :: String,
    expiresIn :: Int,
    scope :: String,
    tokenType :: String
  }
  deriving (Eq, Generic, Show)

instance FromJSON NewAccessTokenResponse where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_', unwrapUnaryRecords = True}

instance ToJSON NewAccessTokenResponse
