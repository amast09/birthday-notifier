{-# LANGUAGE OverloadedStrings #-}

module OauthRefreshToken (RefreshTokenRow (..), getTokens, insertToken) where

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

-- TODO: How do DB failures propagate?
getTokens :: Connection -> IO [RefreshTokenRow]
getTokens c = do
  query_ c "SELECT email, refresh_token FROM google_oauth_refresh_token"

insertToken :: Connection -> RefreshTokenRow -> IO Int64
insertToken c = execute c "INSERT INTO google_oauth_refresh_token (email, refresh_token) VALUES (?, ?)"
