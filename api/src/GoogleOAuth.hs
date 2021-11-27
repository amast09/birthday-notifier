{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveGeneric #-}

module GoogleOAuth ( getOAuthCredentials, OAuthTokenData ) where

import           Data.Aeson
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status  (statusCode)
import GHC.Generics
import Data.Aeson.Types (Parser)
import System.Environment (getEnv)

oauthUrl :: String
oauthUrl = "https://oauth2.googleapis.com/token"
grantType :: String
grantType = "authorization_code"
redirectUri :: String
redirectUri = "http://localhost:3001/google-oauth-callback"

getOAuthCredentials :: String -> IO (Either String OAuthTokenData)
getOAuthCredentials code = do
  manager <- newManager tlsManagerSettings
  clientId <- getEnv "GOOGLE_OAUTH_CLIENT_ID"
  clientSecret <- getEnv "GOOGLE_OAUTH_CLIENT_SECRET"
  let requestObject = object
          [ "client_id" .= clientId
          , "client_secret" .= clientSecret
          , "code" .= code
          , "grant_type" .= grantType
          , "redirect_uri" .= redirectUri
          ]
  initialRequest <- parseRequest oauthUrl
  let request = initialRequest
          { method = "POST"
          , requestBody = RequestBodyLBS $ encode requestObject
          , requestHeaders = [("Content-Type", "application/json; charset=utf-8")]
          }

  response <- httpLbs request manager
  let body = responseBody response
  print "----------------------------------------------------------------------"
  print $ "Making request to: " ++ redirectUri
  print $ "Response status code: " ++ show (statusCode $ responseStatus response)
  print $ "Response body:" ++ show body
  return (eitherDecode body :: Either String OAuthTokenData)


data OAuthTokenData = OAuthTokenData {accessToken :: String
  , expiresIn :: Int
  , refreshToken :: String
  , scope :: String
  , tokenType :: String} deriving (Eq, Generic, Show)

instance FromJSON OAuthTokenData where
  parseJSON = withObject "OAuthTokenData" oAuthTokenDataParser

oAuthTokenDataParser :: Object -> Parser OAuthTokenData
oAuthTokenDataParser obj = do
  parsedAccessToken <- obj .: "access_token"
  parsedExpiresIn <- obj .: "expires_in"
  parsedRefreshToken <- obj .: "refresh_token"
  parsedScope <- obj .: "scope"
  parsedTokenType <- obj .: "token_type"
  return (OAuthTokenData {accessToken = parsedAccessToken
    , expiresIn = parsedExpiresIn
    , refreshToken = parsedRefreshToken
    , scope = parsedScope
    , tokenType = parsedTokenType})

instance ToJSON OAuthTokenData
