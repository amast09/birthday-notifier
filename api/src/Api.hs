{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api (Api.run) where

import qualified RefreshTokenResponse as ATR
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
import qualified OauthRefreshToken as ORT
import SendGrid as SG
import Servant
import System.Environment (getEnv)
import System.IO
import Jose.Jwk (JwkSet)

type BirthdayNotifierApi =
  "google-oauth-callback" :> QueryParam "code" String :> QueryParam "error" String :> Get '[JSON] HandlerResult

usersApi :: Proxy BirthdayNotifierApi
usersApi = Proxy

run :: IO ()
run = do
  postgresHost <- getEnv "POSTGRES_HOST"
  postgresDatabase <- getEnv "POSTGRES_DB"
  postgresUser <- getEnv "POSTGRES_USER"
  postgresPassword <- getEnv "POSTGRES_PASSWORD"
  stringPort <- getEnv "API_PORT"

  let connInfo =
        defaultConnectInfo
          { connectHost = postgresHost,
            connectDatabase = postgresDatabase,
            connectUser = postgresUser,
            connectPassword = postgresPassword
          }

  pgConn <- connect connInfo

  -- TODO: Handle a string that is not an integer
  let port = read stringPort :: Int
  let settings =
        setPort port $
          setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) defaultSettings
  runSettings settings =<< mkApp pgConn

mkApp :: Connection -> IO Application
mkApp conn = return $ serve usersApi (server conn)

server :: Connection -> Server BirthdayNotifierApi
server = handleOauthCallback

handleOauthCallback :: Connection -> Maybe String -> Maybe String -> Handler HandlerResult
handleOauthCallback conn (Just code) Nothing = do
  rawResult <- liftIO $ getRefreshToken code
  jwkSet <- liftIO $ getJwkKeys ()
  insertionResult <- liftIO $ saveTokenToDb conn jwkSet rawResult
  return (parseResult insertionResult)
handleOauthCallback _ Nothing (Just e) = return HandlerResult {result = e}
handleOauthCallback _ _ _ = throwError err400

parseResult :: Show a => Either String a -> HandlerResult
parseResult (Right r) = HandlerResult {result = show r}
parseResult (Left e) = HandlerResult {result = e}


saveTokenToDb :: Connection -> Either String JwkSet -> Either String ATR.RefreshTokenResponse -> IO (Either String Int64)
saveTokenToDb _ (Left e) _ = pure (Left e)
saveTokenToDb _ _ (Left e) = pure (Left e)
saveTokenToDb conn (Right jwkSet) (Right at) = do
  maybeEmail <- ATR.getEmailAddress jwkSet at
  let maybeRefreshTokenRow = fmap (\email -> ORT.RefreshTokenRow {ORT.refresh_token = ATR.accessToken at, ORT.email = email}) maybeEmail
  case maybeRefreshTokenRow of
        Right row -> fmap Right (ORT.insertToken conn row)
        Left e -> pure (Left (show e))

newtype HandlerResult = HandlerResult {result :: String} deriving (Eq, Show, Generic)

instance ToJSON HandlerResult
