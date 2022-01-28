{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module AccessTokensResponse (AccessTokensResponse(..), getEmailAddress) where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LS
import qualified Jose.Jwk as JWK
import qualified Jose.Jwt as JWT
import qualified Jose.Jwa as JWA

data EmailJwtPayload = EmailJwtPayload { email :: String } deriving (Show, Generic, ToJSON, FromJSON)

data EmailParseError = FailedToCreateJwk | UnexpectedJwtContent | UnexpectedJwtPayload | JwtError JWT.JwtError deriving (Show, Eq)

parseJwtPayload :: C.ByteString -> Either EmailParseError String
parseJwtPayload jwtJsonPayload =
  let maybeParsedPayload = (decode $ LS.fromStrict jwtJsonPayload) :: Maybe EmailJwtPayload
      nxt = case maybeParsedPayload of
        Just payload -> Right $ email payload
        Nothing -> Left UnexpectedJwtPayload
  in nxt

parseDecodedJwt :: JWT.JwtContent -> Either EmailParseError String
parseDecodedJwt (JWT.Jws (_, d)) = parseJwtPayload d
parseDecodedJwt _ = Left UnexpectedJwtContent

decodeJwt :: Maybe JWK.Jwk -> JWT.JwtEncoding -> C.ByteString -> IO(Either EmailParseError JWT.JwtContent)
decodeJwt (Just jwk) encoding jwt = do
  let decodedIO = (JWT.decode [jwk] (Just encoding) jwt) :: IO (Either JWT.JwtError JWT.JwtContent)
  decoded <- decodedIO
  let result = case decoded of
        Left(e) -> Left (JwtError e)
        Right(r) -> Right r
  return result
decodeJwt Nothing _ _ = pure $ Left FailedToCreateJwk

getEmailAddress :: AccessTokensResponse -> IO (Either EmailParseError String)
getEmailAddress at = do
  let jwkJsonString = "{\"kid\":\"9eaa026f635157dfad532e4183a6b823d752ad1d\", \"e\": \"AQAB\",\"kty\": \"RSA\",\"n\": \"g1EnTMZHGXJ-be3b-kBNyKQpZHTELgcnKtuK8Em1Slkc38kjiSlTYQ5JDAC5S2pT6ithzGIl1zyx7w8_YiXKv3N6TIbResKZS07okJCzrCQsABvAObcLGhVo9c8faaOv-U9ydwEhAusO9Aq0qG8UG8xtKJkRk9WTU8yHgWqOgv-zRvQ8dp9VBKofEpojFXFKUtV2DHgVere8dzoJk7zWi5dyQ48Ypy9Vx-nbHO1a8oHttGRDvWn9nbcxi88zCv7ik0IrjIENkQxEekUyseD7vdczqFsu2O3z5b465bjJc16fajYLQQH2gnjltwai0heRKH9Sfpww7ViCYNIXCk8v0w\"}" :: C.ByteString
  let jwk = decodeStrict jwkJsonString :: Maybe JWK.Jwk
  let encoding = JWT.JwsEncoding JWA.RS256
  let jwtString = C.pack (idToken at)
  decodedJwt <- decodeJwt jwk encoding jwtString
  return $ decodedJwt >>= parseDecodedJwt

data AccessTokensResponse = AccessTokensResponse
  { accessToken :: String,
    expiresIn :: Int,
    refreshToken :: String,
    idToken :: String,
    scope :: String,
    tokenType :: String
  }
  deriving (Eq, Generic, Show)

instance FromJSON AccessTokensResponse where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance ToJSON AccessTokensResponse
