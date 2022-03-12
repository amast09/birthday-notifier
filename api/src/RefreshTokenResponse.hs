{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module RefreshTokenResponse (RefreshTokenResponse (..), getEmailAddress, EmailParseError) where

import Data.Aeson
  ( FromJSON (parseJSON),
    Options (fieldLabelModifier),
    ToJSON,
    camelTo2,
    decode,
    decodeStrict,
    defaultOptions,
    genericParseJSON,
  )
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LS
import qualified Data.String as S
import GHC.Generics
import qualified Jose.Jwa as JWA
import qualified Jose.Jwk as JWK
import qualified Jose.Jwt as JWT

newtype EmailJwtPayload = EmailJwtPayload {email :: String} deriving (Show, Generic, ToJSON, FromJSON)

data EmailParseError = BadJwkSet | UnexpectedJwtContent | UnexpectedJwtPayload | JwtError JWT.JwtError deriving (Show, Eq)

parseJwtPayload :: C.ByteString -> Either EmailParseError String
parseJwtPayload jwtJsonPayload =
  let maybeParsedPayload = (decode $ LS.fromStrict jwtJsonPayload) :: Maybe EmailJwtPayload
      nxt = case maybeParsedPayload of
        Just payload -> Right $ email payload
        Nothing -> Left UnexpectedJwtPayload
   in nxt

parseJwkSetPayload :: C.ByteString -> Maybe JWK.JwkSet
parseJwkSetPayload jwkSetJsonPayload =
  (decode $ LS.fromStrict jwkSetJsonPayload) :: Maybe JWK.JwkSet

parseDecodedJwt :: JWT.JwtContent -> Either EmailParseError String
parseDecodedJwt (JWT.Jws (_, d)) = parseJwtPayload d
parseDecodedJwt _ = Left UnexpectedJwtContent

decodeJwt :: JWK.JwkSet -> JWT.JwtEncoding -> C.ByteString -> IO (Either EmailParseError JWT.JwtContent)
decodeJwt jwkSet encoding jwt = do
  let jwkKeys = JWK.keys jwkSet
  let decodedIO = JWT.decode jwkKeys (Just encoding) jwt :: IO (Either JWT.JwtError JWT.JwtContent)
  decoded <- decodedIO
  let result = case decoded of
        Left e -> Left (JwtError e)
        Right r -> Right r
  return result

getEmailAddress :: JWK.JwkSet -> RefreshTokenResponse -> IO (Either EmailParseError String)
getEmailAddress jwkSet refreshTokenResponse = do
  let encoding = JWT.JwsEncoding JWA.RS256
  let jwtString = C.pack . idToken $ refreshTokenResponse
  decodedJwt <- decodeJwt jwkSet encoding jwtString
  return $ decodedJwt >>= parseDecodedJwt

data RefreshTokenResponse = RefreshTokenResponse
  { accessToken :: String,
    expiresIn :: Int,
    refreshToken :: String,
    idToken :: String,
    scope :: String,
    tokenType :: String
  }
  deriving (Eq, Generic, Show)

instance FromJSON RefreshTokenResponse where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance ToJSON RefreshTokenResponse
