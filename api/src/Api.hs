{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Data.Aeson
import           Data.Either
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
import Control.Monad.IO.Class

import GoogleOAuth

type BirthdayNotifierApi =
  "google-oauth-callback" :> QueryParam "code" String :> QueryParam "error" String :> Get '[JSON] OAuthResult

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

handleOauthCallback :: Maybe String -> Maybe String -> Handler OAuthResult
handleOauthCallback (Just code) Nothing = do
  rawResult <- liftIO (getOAuthCredentials code)
  nextToken <- liftIO (getNextToken rawResult)
  return (OAuthResult {result = nextToken})
handleOauthCallback Nothing (Just e) = return (OAuthResult e)
handleOauthCallback _ _ = throwError err400

parseResult :: Either String OAuthTokenData -> OAuthResult
parseResult (Right r) = OAuthResult {result = refreshToken r}
parseResult (Left e) = OAuthResult {result = e}

getNextToken :: Either String OAuthTokenData -> IO String
getNextToken (Right r) = getNewAuthToken $ refreshToken r
getNextToken _ = pure "foobar"

newtype OAuthResult = OAuthResult {result :: String} deriving (Eq, Show, Generic)

instance ToJSON OAuthResult
