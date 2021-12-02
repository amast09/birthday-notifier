{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import AccessTokensResponse (AccessTokensResponse, refreshToken)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Either
import GHC.Generics
import GoogleOAuth
import GooglePeople (getConnections)
import Network.Wai
import Network.Wai.Handler.Warp
import NewAccessTokenResponse (NewAccessTokenResponse, accessToken)
import Servant
import System.IO
import ConnectionsResponse (ConnectionsResponse, connections)

type BirthdayNotifierApi =
  "google-oauth-callback" :> QueryParam "code" String :> QueryParam "error" String :> Get '[JSON] HandlerResult

usersApi :: Proxy BirthdayNotifierApi
usersApi = Proxy

run :: IO ()
run = do
  let port = 3001
      settings =
        setPort port $
          setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve usersApi server

server :: Server BirthdayNotifierApi
server = handleOauthCallback

handleOauthCallback :: Maybe String -> Maybe String -> Handler (HandlerResult)
handleOauthCallback (Just code) Nothing = do
  rawResult <- liftIO (getAccessTokens code)
  nextToken <- liftIO (getNextToken rawResult)
  contacts <- liftIO (makeContactsRequest nextToken)
  return (parseResult3 contacts)
handleOauthCallback Nothing (Just e) = return HandlerResult {result = e}
handleOauthCallback _ _ = throwError err400

parseResult1 :: Either String AccessTokensResponse -> HandlerResult
parseResult1 (Right r) = HandlerResult {result = refreshToken r}
parseResult1 (Left e) = HandlerResult {result = e}

parseResult2 :: Either String NewAccessTokenResponse -> HandlerResult
parseResult2 (Right r) = HandlerResult {result = accessToken r}
parseResult2 (Left e) = HandlerResult {result = e}

parseResult3 :: Either String ConnectionsResponse -> HandlerResult
parseResult3 (Right r) = HandlerResult {result = (show r)}
parseResult3 (Left e) = HandlerResult {result = e}

getNextToken :: Either String AccessTokensResponse -> IO (Either String NewAccessTokenResponse)
getNextToken (Right r) = getNewAccessToken $ refreshToken r
getNextToken (Left l) = pure (Left l)

makeContactsRequest :: Either String NewAccessTokenResponse -> IO (Either String ConnectionsResponse)
makeContactsRequest (Right r) = getConnections $ accessToken r
makeContactsRequest (Left l) = pure (Left l)

newtype HandlerResult = HandlerResult {result :: String} deriving (Eq, Show, Generic)

instance ToJSON HandlerResult
