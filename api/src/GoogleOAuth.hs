{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module GoogleOAuth (getAccessTokens, getNewAccessToken) where

import AccessTokensResponse (AccessTokensResponse)
import Data.Aeson
import Data.Aeson.Types (Parser)
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import NewAccessTokenResponse (NewAccessTokenResponse)
import System.Environment (getEnv)

oauthUrl :: String
oauthUrl = "https://oauth2.googleapis.com/token"

authCodeGrantType :: String
authCodeGrantType = "authorization_code"

refreshTokenGrantType :: String
refreshTokenGrantType = "refresh_token"

redirectUri :: String
redirectUri = "http://localhost:3001/google-oauth-callback"

getAccessTokens :: String -> IO (Either String AccessTokensResponse)
getAccessTokens code = do
  manager <- newManager tlsManagerSettings
  clientId <- getEnv "GOOGLE_OAUTH_CLIENT_ID"
  clientSecret <- getEnv "GOOGLE_OAUTH_CLIENT_SECRET"
  let requestObject =
        object
          [ "client_id" .= clientId,
            "client_secret" .= clientSecret,
            "code" .= code,
            "grant_type" .= authCodeGrantType,
            "redirect_uri" .= redirectUri
          ]
  initialRequest <- parseRequest oauthUrl
  let request =
        initialRequest
          { method = "POST",
            requestBody = RequestBodyLBS $ encode requestObject,
            requestHeaders = [("Content-Type", "application/json; charset=utf-8")]
          }

  response <- httpLbs request manager
  let body = responseBody response
  print "----------------------------------------------------------------------"
  print $ "Making request for refresh token to: " ++ oauthUrl
  print $ "Response status code: " ++ show (statusCode $ responseStatus response)
  print $ "Response body:" ++ show body
  return (eitherDecode body :: Either String AccessTokensResponse)

getNewAccessToken :: String -> IO (Either String NewAccessTokenResponse)
getNewAccessToken oauthRefreshToken = do
  manager <- newManager tlsManagerSettings
  clientId <- getEnv "GOOGLE_OAUTH_CLIENT_ID"
  clientSecret <- getEnv "GOOGLE_OAUTH_CLIENT_SECRET"
  let requestObject =
        object
          [ "client_id" .= clientId,
            "client_secret" .= clientSecret,
            "grant_type" .= refreshTokenGrantType,
            "refresh_token" .= oauthRefreshToken
          ]
  initialRequest <- parseRequest oauthUrl
  let request =
        initialRequest
          { method = "POST",
            requestBody = RequestBodyLBS $ encode requestObject,
            requestHeaders = [("Content-Type", "application/json; charset=utf-8")]
          }

  response <- httpLbs request manager
  let body = responseBody response
  print "----------------------------------------------------------------------"
  print $ "Making request to refresh oauth token to: " ++ oauthUrl
  print $ "Response status code: " ++ show (statusCode $ responseStatus response)
  print $ "Response body:" ++ show body
  return (eitherDecode body :: Either String NewAccessTokenResponse)
