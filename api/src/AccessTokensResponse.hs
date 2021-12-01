{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AccessTokensResponse (AccessTokensResponse, refreshToken) where

import Data.Aeson
import GHC.Generics

data AccessTokensResponse = AccessTokensResponse
  { accessToken :: String,
    expiresIn :: Int,
    refreshToken :: String,
    scope :: String,
    tokenType :: String
  }
  deriving (Eq, Generic, Show)

instance FromJSON AccessTokensResponse where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance ToJSON AccessTokensResponse
