{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module GooglePeople (getConnections) where

import ConnectionsResponse
import Data.Aeson
import Data.ByteString.Char8 (pack)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import System.IO (hPutStrLn, stderr)

peopleUrl :: String
peopleUrl = "https://people.googleapis.com/v1/people/me/connections"

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
  hPutStrLn stderr "----------------------------------------------------------------------"
  hPutStrLn stderr $ "Making request to get connections to: " ++ peopleUrl
  hPutStrLn stderr $ "Response status code: " ++ show (statusCode $ responseStatus response)
  hPutStrLn stderr $ "Response body:" ++ show body
  return (eitherDecode body :: Either String ConnectionsResponse)
