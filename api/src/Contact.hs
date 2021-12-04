{-# LANGUAGE DeriveGeneric #-}

module Contact (Contact, contactsFromConnectionsResponse) where

import Data.Aeson
import Data.Maybe
import GHC.Generics
import ConnectionsResponse as CR
import Control.Applicative (liftA2)

data Contact = Contact { name :: String, birthday :: Day } deriving (Eq, Generic, Show)
instance FromJSON Contact
instance ToJSON Contact

makeContact :: String -> Day -> Contact
makeContact n bd = Contact { name = n, birthday = bd }

-- TODO: Could move these into their own file
maybeHead :: [a] -> Maybe a
maybeHead (a:_) = Just a
maybeHead _ = Nothing

headFromMaybeList :: Maybe [a] -> Maybe a
headFromMaybeList maybeList = maybeList >>= maybeHead

connectionToContact :: CR.Connection -> Maybe Contact
connectionToContact c =
  let maybeName = (headFromMaybeList $ names c) >>= CR.displayName
      maybeBirthday = (headFromMaybeList $ birthdays c) >>= CR.date
  in liftA2 makeContact maybeName maybeBirthday

foldConn :: [Contact] -> Maybe Contact -> [Contact]
foldConn cs (Just c) = cs ++ [c]
foldConn cs Nothing = cs

contactsFromConnectionsResponse :: CR.ConnectionsResponse -> [Contact]
contactsFromConnectionsResponse cr = foldl foldConn [] $ connectionToContact <$> CR.connections cr
