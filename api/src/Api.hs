{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO

type BirthdayNotifierApi =
  "google-oauth-callback" :> QueryParam "code" String :> QueryParam "error" String :> Get '[JSON] OauthResult

usersApi :: Proxy BirthdayNotifierApi
usersApi = Proxy

run :: IO ()
run = do
  let port = 3001
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve usersApi server

server :: Server BirthdayNotifierApi
server = handleOauthCallback

handleOauthCallback :: Maybe String -> Maybe String -> Handler OauthResult
handleOauthCallback (Just code) Nothing = return (OauthResult code)
handleOauthCallback Nothing (Just error) = return (OauthResult error)
handleOauthCallback _ _ = throwError err400

data OauthResult = OauthResult { result :: String } deriving (Eq, Show, Generic)

instance ToJSON OauthResult
