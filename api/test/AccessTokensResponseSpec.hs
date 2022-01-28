module AccessTokensResponseSpec (spec) where

import AccessTokensResponse
import Test.Hspec

-- TODO -> Add tests that exercise all failure cases
spec :: Spec
spec = do
  describe "getEmailAddress" $ do
    it "returns Just(emailAddress) from an access token" $ do
      let at =
            AccessTokensResponse
              { accessToken = "access token",
                expiresIn = 10,
                refreshToken = "refresh token",
                idToken = "eyJhbGciOiJSUzI1NiIsImtpZCI6IjllYWEwMjZmNjM1MTU3ZGZhZDUzMmU0MTgzYTZiODIzZDc1MmFkMWQiLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJodHRwczovL2FjY291bnRzLmdvb2dsZS5jb20iLCJhenAiOiI4NzczMzc5NjAzODctMmpwa3RvOWxsazVyOHNxN2EwZTd1ODJyZTAwNHFqaHUuYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJhdWQiOiI4NzczMzc5NjAzODctMmpwa3RvOWxsazVyOHNxN2EwZTd1ODJyZTAwNHFqaHUuYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJzdWIiOiIxMDkxNDUzMjcwNTQ3NzUzODg2NDciLCJlbWFpbCI6ImFtYXN0MDlAZ21haWwuY29tIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsImF0X2hhc2giOiIxN2h2aGNrSW1GMjM3cTZ0LUFyRl9RIiwiaWF0IjoxNjQzMzg0NDI4LCJleHAiOjE2NDMzODgwMjh9.L4Z-EbIshoV9PaU3fgPNAHUaBy1q_YN792oemi0XNX3z05hC-TZrJasKyvR34L_nuMgMeIW4ROiFROy135N-PzBgbV9Hl4FAYmSct6REr2U_R8u8m4x3ex3GDLJOP4vDLq3aqadMSNZeD8JCKtOj5SBiMfgtSbrNfhXUZY6yc6MlnQgRRPqv8iTfS0gY98tbCprTnfWCkKA9_1IYPo6CdkPfByGMIwu4eUpekZn5IVMgIGKZwDDtW3I2IpbgmtePc_QT_lFXMC7fx60MdXm_A55v_QX6-LVQPb9R0GUpdPZNaw0Awn6_NjZasVRKUHbJxpxsAzIsJzPV-oVBFUkdEQ",
                scope = "scope",
                tokenType = "token type"
              }
      result <- getEmailAddress at
      result `shouldBe` Right "amast09@gmail.com"
