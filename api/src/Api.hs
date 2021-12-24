{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import AccessTokensResponse (AccessTokensResponse, refreshToken)
import ConnectionsResponse (ConnectionsResponse, connections)
import Contact (Contact, contactsFromConnectionsResponse, createBirthdayEmailMessage, name)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Either
import Data.Int (Int64)
import Data.Time.Clock
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Generics
import GoogleOAuth
import GooglePeople (getConnections)
import Network.Wai
import Network.Wai.Handler.Warp
import NewAccessTokenResponse (NewAccessTokenResponse, accessToken)
import SendGrid as SG
import Servant
import System.Environment (getEnv)
import System.IO

type BirthdayNotifierApi =
  "google-oauth-callback" :> QueryParam "code" String :> QueryParam "error" String :> Get '[JSON] HandlerResult

usersApi :: Proxy BirthdayNotifierApi
usersApi = Proxy

data RefreshTokenRow = RefreshTokenRow {email :: String, refresh_token :: String}
  deriving (Show)

instance FromRow RefreshTokenRow where
  fromRow = RefreshTokenRow <$> field <*> field

instance ToRow RefreshTokenRow where
  toRow t = [toField (email t), toField (refresh_token t)]

allRefreshTokens :: Connection -> IO [RefreshTokenRow]
allRefreshTokens c = query_ c "SELECT email, refresh_token FROM google_oauth_refresh_token"

insertRefreshToken :: Connection -> RefreshTokenRow -> IO Int64
insertRefreshToken c = execute c "INSERT INTO google_oauth_refresh_token (email, refresh_token) VALUES (?, ?)"

run :: IO ()
run = do
  let localPG =
        defaultConnectInfo
          { connectHost = "localhost",
            connectDatabase = "birthday-notifier-database",
            connectUser = "birthday-notifier-user",
            connectPassword = "birthday-notifier-password"
          }

  conn <- connect localPG
  insertResult <- insertRefreshToken conn (RefreshTokenRow {email = "amast09@gmail.com", refresh_token = "refresh token!"})
  abc <- allRefreshTokens conn
  _ <- print abc

  port <- getEnv "API_PORT"
  let settings =
        setPort port $
          setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve usersApi server

server :: Server BirthdayNotifierApi
server = handleOauthCallback

handleOauthCallback :: Maybe String -> Maybe String -> Handler HandlerResult
handleOauthCallback (Just code) Nothing = do
  rawResult <- liftIO $ getAccessTokens code
  nextToken <- liftIO $ getNextToken rawResult
  connectionsResponse <- liftIO $ makeContactsRequest nextToken

  let contacts = fmap contactsFromConnectionsResponse connectionsResponse
  let numberOfContacts = fmap length contacts

  now <- liftIO getCurrentTime
  _ <- liftIO $ sendBirthdayEmail now contacts

  return (parseResult numberOfContacts)
handleOauthCallback Nothing (Just e) = return HandlerResult {result = e}
handleOauthCallback _ _ = throwError err400

parseResult :: Show a => Either String a -> HandlerResult
parseResult (Right r) = HandlerResult {result = show r}
parseResult (Left e) = HandlerResult {result = e}

sendBirthdayEmail :: UTCTime -> Either String [Contact] -> IO ()
sendBirthdayEmail now (Right contacts) = do
  let birthdayMessage = createBirthdayEmailMessage now contacts
  sendEmail
    SG.SendEmailParams
      { SG.emailSubject = "Today's Birthdays!",
        SG.emailContent = birthdayMessage,
        SG.emailToAddress = "amast09@gmail.com"
      }

contactsWithBirthdaysToday _ _ = pure ()

getNextToken :: Either String AccessTokensResponse -> IO (Either String NewAccessTokenResponse)
getNextToken (Right r) = getNewAccessToken $ refreshToken r
getNextToken (Left l) = pure (Left l)

makeContactsRequest :: Either String NewAccessTokenResponse -> IO (Either String ConnectionsResponse)
makeContactsRequest (Right r) = getConnections $ accessToken r
makeContactsRequest (Left l) = pure (Left l)

newtype HandlerResult = HandlerResult {result :: String} deriving (Eq, Show, Generic)

instance ToJSON HandlerResult
