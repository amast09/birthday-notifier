{-# LANGUAGE OverloadedStrings #-}

module OauthRefreshToken (getTokens, insertToken) where

import Data.Int (Int64)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

data RefreshTokenRow = RefreshTokenRow {email :: String, refresh_token :: String}
  deriving (Show)

instance FromRow RefreshTokenRow where
  fromRow = RefreshTokenRow <$> field <*> field

instance ToRow RefreshTokenRow where
  toRow t = [toField (email t), toField (refresh_token t)]

getTokens :: Connection -> IO [String]
getTokens c = do
  refreshTokens <- query_ c "SELECT email, refresh_token FROM google_oauth_refresh_token"
  return (fmap refresh_token refreshTokens)

insertToken :: Connection -> RefreshTokenRow -> IO Int64
insertToken c = execute c "INSERT INTO google_oauth_refresh_token (email, refresh_token) VALUES (?, ?)"
