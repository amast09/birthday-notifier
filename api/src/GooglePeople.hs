{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module GooglePeople (getContacts) where

import AccessTokensResponse (AccessTokensResponse)
import Data.Aeson
import Data.Aeson.Types (Parser)
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import NewAccessTokenResponse (NewAccessTokenResponse)
import Data.ByteString.Char8
import System.Environment (getEnv)


peopleUrl :: String
peopleUrl = "https://people.googleapis.com/v1/people/me/connections"

getContacts :: String -> IO ()
getContacts accessToken = do
  manager <- newManager tlsManagerSettings

  initialRequest <- parseRequest peopleUrl
  let request =
        initialRequest
          { method = "GET",
            queryString = (pack ("access_token=" ++ accessToken ++ "&pageSize=200&personFields=names,emailAddresses")),
            requestHeaders = [("Content-Type", "application/json; charset=utf-8")]
          }

  response <- httpLbs request manager
  let body = responseBody response
  print "----------------------------------------------------------------------"
  print $ "Making request for refresh token to: " ++ peopleUrl
  print $ "Response status code: " ++ show (statusCode $ responseStatus response)
  print $ "Response body:" ++ show body
