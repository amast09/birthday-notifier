{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module SendGrid (sendEmail, SendEmailParams (..)) where

import Data.Aeson
import Data.Aeson.TH (Options (fieldLabelModifier), defaultOptions, deriveJSON)
import Data.ByteString.Char8
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import System.Environment (getEnv)

sendMailUrl :: String
sendMailUrl = "https://api.sendgrid.com/v3/mail/send"

data From = From {fromEmail :: String, name :: String} deriving (Eq, Generic, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = \x -> if x == "fromEmail" then "email" else x} ''From)

data Content = Content {contentType :: String, value :: String} deriving (Eq, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = \x -> if x == "contentType" then "type" else x} ''Content)

data ToPersonalization = ToPersonalization {email :: String} deriving (Eq, Generic, Show)

instance FromJSON ToPersonalization

instance ToJSON ToPersonalization

data Personalization = Personalization {toPersonalization :: [ToPersonalization]} deriving (Eq, Generic, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = \x -> if x == "toPersonalization" then "to" else x} ''Personalization)

data SendMailBody = SendMailBody
  { personalizations :: [Personalization],
    sendMailBodyFrom :: From,
    subject :: String,
    content :: [Content]
  }
  deriving (Eq, Generic, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = \x -> if x == "sendMailBodyFrom" then "from" else x} ''SendMailBody)

data SendEmailParams = SendEmailParams
  { emailSubject :: String,
    emailContent :: String,
    emailToAddress :: String
  }

sendEmail :: SendEmailParams -> IO ()
sendEmail params = do
  manager <- newManager tlsManagerSettings
  sendGridApiKey <- getEnv "SENDGRID_API_KEY"
  let requestBody =
        SendMailBody
          { personalizations =
              [ Personalization
                  { toPersonalization =
                      [ ToPersonalization
                          { email = emailToAddress params
                          }
                      ]
                  }
              ],
            sendMailBodyFrom = From {fromEmail = "amast09@gmail.com", name = "Birthday Notifier"},
            subject = emailSubject params,
            content = [Content {contentType = "text/plain", value = emailContent params}]
          }
  initialRequest <- parseRequest sendMailUrl
  let request =
        initialRequest
          { method = "POST",
            requestBody = RequestBodyLBS $ encode requestBody,
            requestHeaders =
              [ ("Content-Type", "application/json"),
                ("Authorization", pack ("Bearer " ++ sendGridApiKey))
              ]
          }

  response <- httpLbs request manager
  let body = responseBody response
  print "----------------------------------------------------------------------"
  print $ "Making request for refresh token to: " ++ sendMailUrl
  print $ "Response status code: " ++ show (statusCode $ responseStatus response)
  print $ "Response body:" ++ show body
