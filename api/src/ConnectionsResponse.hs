{-# LANGUAGE DeriveGeneric #-}

module ConnectionsResponse (Connection, ConnectionsResponse, Day (..), connections, names, birthdays, displayName, date) where

import Data.Aeson
import GHC.Generics

data Day = Day {year :: Maybe Int, month :: Int, day :: Int} deriving (Eq, Generic, Show)

instance FromJSON Day

instance ToJSON Day

data Birthday = Birthday {date :: Maybe Day} deriving (Eq, Generic, Show)

instance FromJSON Birthday

instance ToJSON Birthday

data Name = Name {displayName :: Maybe String} deriving (Eq, Generic, Show)

instance FromJSON Name

instance ToJSON Name

data Connection = Connection {names :: Maybe [Name], birthdays :: Maybe [Birthday]} deriving (Eq, Generic, Show)

instance FromJSON Connection

instance ToJSON Connection

data ConnectionsResponse = ConnectionsResponse {connections :: [Connection]} deriving (Eq, Generic, Show)

instance FromJSON ConnectionsResponse

instance ToJSON ConnectionsResponse
