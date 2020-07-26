import Oauth2Client from "googleapis-common";
import { google } from "googleapis";
import { Credentials } from "google-auth-library/build/src/auth/credentials";

const createOauthClient = () =>
  new google.auth.OAuth2(
    process.env.GOOGLE_PEOPLE_API_CLIENT_ID,
    process.env.GOOGLE_PEOPLE_API_CLIENT_SECRET,
    process.env.GOOGLE_PEOPLE_API_REDIRECT_URL
  );

const getAuthUrl = (): string => {
  const oAuthClient = createOauthClient();

  return oAuthClient.generateAuthUrl({
    access_type: "offline",
    scope: ["https://www.googleapis.com/auth/contacts.readonly"],
  });
};

const getOAuthClientCredentials = (tokenCode: string): Promise<Credentials> => {
  const oAuthClient = createOauthClient();

  return oAuthClient
    .getToken(tokenCode)
    .then<Credentials>((tokenResponse) => {
      return tokenResponse.tokens;
    })
    .catch((e) => {
      console.error("Error retrieving access token", e);
      throw e;
    });
};

const getOauthClientForCredentials = (
  credentials: Credentials
): Oauth2Client.OAuth2Client => {
  const oauthClient = createOauthClient();
  oauthClient.setCredentials(credentials);
  return oauthClient;
};

export default {
  getOAuthClientCredentials,
  getAuthUrl,
  getOauthClientForCredentials,
};
