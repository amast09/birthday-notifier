{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module AccessTokensResponse ( AccessTokensResponse, refreshToken ) where

import GHC.Generics
import           Data.Aeson

data AccessTokensResponse = AccessTokensResponse {accessToken :: String
  , expiresIn :: Int
  , refreshToken :: String
  , scope :: String
  , tokenType :: String} deriving (Eq, Generic, Show)

instance FromJSON AccessTokensResponse where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance ToJSON AccessTokensResponse
