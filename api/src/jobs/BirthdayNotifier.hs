module BirthdayNotifier (sendDailyBirthdayEmail) where

import Contact (Contact, contactsFromConnectionsResponse, createBirthdayEmailMessage)
import Data.Time (UTCTime, getCurrentTime)
import Database.PostgreSQL.Simple (ConnectInfo (connectDatabase, connectPassword, connectUser), connect, connectHost, defaultConnectInfo)
import GoogleOAuth (getNewAccessToken)
import GooglePeople (getConnections)
import NewAccessTokenResponse (NewAccessTokenResponse (accessToken))
import OauthRefreshToken (RefreshTokenRow (email, refresh_token), getTokens)
import SendGrid (SendEmailParams (SendEmailParams, emailContent, emailSubject, emailToAddress), sendEmail)
import System.Environment (getEnv)
import System.IO (hPutStrLn, stderr)

data DailyBirthdayEmailError = FailedToFetchNewAccessToken | FailedToFetchConnections deriving (Show, Eq)

sendDailyBirthdayEmail :: IO ()
sendDailyBirthdayEmail = do
  postgresHost <- getEnv "POSTGRES_HOST"
  postgresDatabase <- getEnv "POSTGRES_DB"
  postgresUser <- getEnv "POSTGRES_USER"
  postgresPassword <- getEnv "POSTGRES_PASSWORD"

  let connInfo =
        defaultConnectInfo
          { connectHost = postgresHost,
            connectDatabase = postgresDatabase,
            connectUser = postgresUser,
            connectPassword = postgresPassword
          }

  postgresConnection <- connect connInfo
  refreshTokens <- getTokens postgresConnection
  accessTokensWithEmails <- mapM accessTokenForRefreshToken refreshTokens
  accessTokensWithContacts <- mapM getContactsForAccessToken accessTokensWithEmails
  currentTime <- getCurrentTime
  mapM_ (sendEmailForAccessToken currentTime) accessTokensWithContacts

accessTokenForRefreshToken :: RefreshTokenRow -> IO (Either DailyBirthdayEmailError (String, NewAccessTokenResponse))
accessTokenForRefreshToken refreshTokenRow = do
  hPutStrLn stderr "Getting Access Token for Refresh Token"
  let refreshTokenString = refresh_token refreshTokenRow
  let emailForToken = email refreshTokenRow
  newAccessToken <- getNewAccessToken refreshTokenString
  return
    ( case newAccessToken of
        Right accessToken -> Right (emailForToken, accessToken)
        Left e -> Left FailedToFetchNewAccessToken
    )

getContactsForAccessToken :: Either DailyBirthdayEmailError (String, NewAccessTokenResponse) -> IO (Either DailyBirthdayEmailError (String, NewAccessTokenResponse, [Contact]))
getContactsForAccessToken (Right (emailForToken, accessTokenResponse)) = do
  hPutStrLn stderr ("Pulling contacts for email " ++ emailForToken)
  connectionResponseForAccessToken <- getConnections $ accessToken accessTokenResponse
  return
    ( case connectionResponseForAccessToken of
        Right connectionsResponse -> Right (emailForToken, accessTokenResponse, contactsFromConnectionsResponse connectionsResponse)
        Left e -> Left FailedToFetchConnections
    )
getContactsForAccessToken (Left e) = pure $ Left e

sendEmailForAccessToken :: UTCTime -> Either DailyBirthdayEmailError (String, NewAccessTokenResponse, [Contact]) -> IO (Either DailyBirthdayEmailError ())
sendEmailForAccessToken birthDate (Right (emailForToken, accessTokenResponse, contacts)) = do
  let maybeBirthdayMessage = createBirthdayEmailMessage birthDate contacts
  case maybeBirthdayMessage of
    Just birthdayMessage -> hPutStrLn stderr ("Sending email to " ++ emailForToken)
    Nothing -> hPutStrLn stderr ("No birthdays for " ++ emailForToken ++ " today, no email being sent")
  case maybeBirthdayMessage of
    Just birthdayMessage -> Right <$> sendEmail SendEmailParams {emailSubject = "Today's Birthdays!", emailContent = birthdayMessage, emailToAddress = emailForToken}
    Nothing -> return $ Right ()
sendEmailForAccessToken _ (Left e) = pure $ Left e
