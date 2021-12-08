{-# LANGUAGE DeriveGeneric #-}

module Contact (Contact (..), contactsFromConnectionsResponse, createBirthdayEmailMessage) where

import ConnectionsResponse as CR
import Control.Applicative (liftA2)
import Data.Aeson
import Data.List
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import GHC.Generics

data Contact = Contact {name :: String, birthday :: CR.Day} deriving (Eq, Generic, Show)

instance FromJSON Contact

instance ToJSON Contact

makeContact :: String -> CR.Day -> Contact
makeContact n bd = Contact {name = n, birthday = bd}

-- TODO: Could move these into their own file
maybeHead :: [a] -> Maybe a
maybeHead (a : _) = Just a
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

isContactsBirthday :: UTCTime -> Contact -> Bool
isContactsBirthday today c =
  let bd = birthday c
      birthdayMonth = month bd
      birthdayDay = day bd
      (_, todayMonth, todayDay) = toGregorian . utctDay $ today
   in todayMonth == birthdayMonth && todayDay == birthdayDay

createBirthdayEmailMessage :: UTCTime -> [Contact] -> String
createBirthdayEmailMessage today cs =
  let contactsWithBirthdaysToday = filter (isContactsBirthday today) cs
      birthdayNames = fmap name contactsWithBirthdaysToday
   in unlines birthdayNames
