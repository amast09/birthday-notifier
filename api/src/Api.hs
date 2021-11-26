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
import Data.Time.Calendar
-- * api

type UsersApi =
  "users" :> Get '[JSON] [User] :<|>
  "users" :> Capture "userId" Integer :> Get '[JSON] User

usersApi :: Proxy UsersApi
usersApi = Proxy

-- * app

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

server :: Server UsersApi
server =
  getUsers :<|>
  getUserById

getUsers :: Handler [User]
getUsers = return allUsers

getUserById :: Integer -> Handler User
getUserById = \ case
  372 -> return user1
  136 -> return user2
  _ -> throwError err404

-- * user

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User

user1 :: User
user1 = User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)

user2 :: User
user2 =  User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)

allUsers :: [User]
allUsers = [user1, user2]
