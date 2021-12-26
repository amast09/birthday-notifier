{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module GooglePeople (getConnections, getUserEmail) where

import ConnectionsResponse
import Data.Aeson
import Data.ByteString.Char8
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)

peopleUrl :: String
peopleUrl = "https://people.googleapis.com/v1/people/me/connections"

userEmailUrl :: String
userEmailUrl = "https://www.googleapis.com/auth/userinfo.email"

getUserEmail :: String -> IO String
getUserEmail accessToken = do
  manager <- newManager tlsManagerSettings

  initialRequest <- parseRequest userEmailUrl
  let request =
        initialRequest
          { method = "GET",
            queryString = pack ("access_token=" ++ accessToken ++ "&pageSize=2000&personFields=names,birthdays"),
            requestHeaders = [("Content-Type", "application/json; charset=utf-8")]
          }

  response <- httpLbs request manager
  let body = responseBody response
  print "----------------------------------------------------------------------"
  print $ "Making request for user email to: " ++ userEmailUrl
  print $ "Response status code: " ++ show (statusCode $ responseStatus response)
  print $ "Response body:" ++ show body
  return (show body)


getConnections :: String -> IO (Either String ConnectionsResponse)
getConnections accessToken = do
  manager <- newManager tlsManagerSettings

  initialRequest <- parseRequest peopleUrl
  let request =
        initialRequest
          { method = "GET",
            queryString = pack ("access_token=" ++ accessToken ++ "&pageSize=2000&personFields=names,birthdays"),
            requestHeaders = [("Content-Type", "application/json; charset=utf-8")]
          }

  response <- httpLbs request manager
  let body = responseBody response
  print "----------------------------------------------------------------------"
  print $ "Making request for refresh token to: " ++ peopleUrl
  print $ "Response status code: " ++ show (statusCode $ responseStatus response)
  print $ "Response body:" ++ show body
  return (eitherDecode body :: Either String ConnectionsResponse)
