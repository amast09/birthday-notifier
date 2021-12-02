{-# LANGUAGE DeriveGeneric #-}

module Contact (Contact) where

import Data.Aeson
import Data.Maybe
import GHC.Generics
import ConnectionsResponse as CR

data Contact = Contact { name :: String, birthday :: Day } deriving (Eq, Generic, Show)
instance FromJSON Contact
instance ToJSON Contact

contactWithAllData :: CR.Connection -> Maybe Contact
contactWithAllData c
  | isJust (CR.names c) = Nothing
  | otherwise = Nothing
contactWithAllData _ = Nothing

--contactsFromConnectionsResponse :: CR.ConnectionsResponse -> [Contact]
--contactsFromConnectionsResponse cr = map $ connections cr