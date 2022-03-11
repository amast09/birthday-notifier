{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module GoogleOAuth (getRefreshToken, getNewAccessToken, getJwkKeys) where

import RefreshTokenResponse (RefreshTokenResponse)
import Data.Aeson
import Data.Aeson.Types (Parser)
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import NewAccessTokenResponse (NewAccessTokenResponse)
import System.Environment (getEnv)
import Jose.Jwk

oauthUrl :: String
oauthUrl = "https://oauth2.googleapis.com/token"

jwkKeysUrl :: String
jwkKeysUrl = "https://www.googleapis.com/oauth2/v3/certs"

authCodeGrantType :: String
authCodeGrantType = "authorization_code"

refreshTokenGrantType :: String
refreshTokenGrantType = "refresh_token"

redirectUri :: String
redirectUri = "http://localhost:3001/google-oauth-callback"

getRefreshToken :: String -> IO (Either String RefreshTokenResponse)
getRefreshToken code = do
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
  return (eitherDecode body :: Either String RefreshTokenResponse)

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

getJwkKeys :: () -> IO (Either String JwkSet)
getJwkKeys _ = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest jwkKeysUrl
  let request = initialRequest { method = "GET", requestHeaders = [("Content-Type", "application/json; charset=utf-8")] }
  response <- httpLbs request manager
  let body = responseBody response
  print "----------------------------------------------------------------------"
  print $ "Making request to refresh oauth token to: " ++ jwkKeysUrl
  print $ "Response status code: " ++ show (statusCode $ responseStatus response)
  print $ "Response body:" ++ show body
  return (eitherDecode body :: Either String JwkSet)
  
