module RefreshTokenResponseSpec (spec) where

import Data.Aeson
import qualified Data.String as S
import Jose.Jwk (JwkSet (JwkSet))
import RefreshTokenResponse
import Test.Hspec

-- TODO -> Add tests that exercise all failure cases
spec :: Spec
spec = do
  describe "getEmailAddress" $ do
    it "returns Just(emailAddress) from an access token with the associated JWK key" $ do
      let expectedEmail = "amast09@gmail.com"
      let jwkKeysJsonString = "{\"keys\":[{\"n\":\"urIBEeEj2HvBoNipv4PcFPGbw66boVQx60hl0sK7rTLKpLZqIkorKiC2d8nDg7Zrm_uYvYBNsoQWZohEsTh3kBSs92BNnbA_Z1Ok345e8BGDKifsi6YuMtjqffIqsZs-gCWE_AxZ_9m-CfCzs5UGgad7E0qFQxlOe18ds-mHhWd3l-CgQsAYNMoII7GCxLsp5GUaPFjld5E9h5dK7LrKH311swII_rypnK6ktduKpcuMLuxcfz8oQ3Gqzp1oZ1fm9eG98adjSLl796vz5Uh-mz__YBkyD67Jibf4pqtQ07skq_Ff7KKQO32I4Yy0Dp7I0aUTYA2ff8JT0Huz2876LQ\",\"e\":\"AQAB\",\"alg\":\"RS256\",\"kty\":\"RSA\",\"use\":\"sig\",\"kid\":\"3dd6ca2a81dc2fea8c3642431e7e296d2d75b446\"},{\"use\":\"sig\",\"e\":\"AQAB\",\"kty\":\"RSA\",\"n\":\"rXzt9xpKC1vqbtVm-XJi2ys1_4LaiRKBhBNyUTtTBZedgJtr3XU6SSol8HEDwzAuPb3cODABr0wpNmEGFg7dcSL6QOSSb3sntvsiYqxUXIFnFpAGMEA2SzconFLdAaLNKAX1T4F1EU50v20EIZFxWdR8sZ0ClrOrixPf_TR2hRoqiyvrpEyeVxxWatae2DPTmgeTmdanPAKjspR9iF4xEpRoo2MKUGGMDDZvFJSSlL1Bd26SbXEHYvn4muOLWuaro4Va2HUPnfDXJEPPAr2Mag1sbiEMgjs0FUlfJkk_oZr8GEOny4TOlhGmJmrPCkunGj3yAmwOmDULpjRihknkpw\",\"alg\":\"RS256\",\"kid\":\"d63dbe73aad88c854de0d8d6c014c36dc25c4292\"}]}"
      let Just jwkSet = decode $ S.fromString jwkKeysJsonString :: Maybe JwkSet
      let refreshTokenResponse =
            RefreshTokenResponse
              { accessToken = "access token",
                expiresIn = 10,
                refreshToken = "refresh token",
                idToken = "eyJhbGciOiJSUzI1NiIsImtpZCI6ImQ2M2RiZTczYWFkODhjODU0ZGUwZDhkNmMwMTRjMzZkYzI1YzQyOTIiLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJodHRwczovL2FjY291bnRzLmdvb2dsZS5jb20iLCJhenAiOiI4NzczMzc5NjAzODctMmpwa3RvOWxsazVyOHNxN2EwZTd1ODJyZTAwNHFqaHUuYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJhdWQiOiI4NzczMzc5NjAzODctMmpwa3RvOWxsazVyOHNxN2EwZTd1ODJyZTAwNHFqaHUuYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJzdWIiOiIxMDkxNDUzMjcwNTQ3NzUzODg2NDciLCJlbWFpbCI6ImFtYXN0MDlAZ21haWwuY29tIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsImF0X2hhc2giOiJReXRoRFFDYVloWFpzVlQtSmo0a3hRIiwiaWF0IjoxNjQ3MDIzNzQyLCJleHAiOjE2NDcwMjczNDJ9.hR0QsRS1TXZZuqD3jWrAyvHF9uH-BKBao-pJtK7VEj1-vbKNL26nsaO9waVo_V1pEbWMhkCGCl8olbfhv--l7Ireo6h0UhR1WpkHZniNrvgnFGuZjgP8k6qMggY5rO-qPJywo6QB4mEYmuh-fXiOwKNwzmXuYX0WEWZXVWUf0OdJySrw9Iy2ej8bTXKUSahLqECkcDOf16ig8C0FF71bmhqRGjgamHZDndSL_Ep4h2Ps8nvGu_PwXIMtJWqPmLtz2IBG34NvkP8RuMlDnhkVd5mSS4h7zLcs2iA7boysqJtKmvW2ejMrqjMpRY-0SLmg538Tl4I7g5uEB_zlWsY-JQ",
                scope = "scope",
                tokenType = "token type"
              }
      result <- getEmailAddress jwkSet refreshTokenResponse
      result `shouldBe` Right expectedEmail
