import Oauth2Client from "googleapis-common";
import { google } from "googleapis";
import { Credentials } from "google-auth-library/build/src/auth/credentials";
import logger from "../logger";
import SubscriberStorage from "../types/SubscriberStorage";

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
    scope: [
      "https://www.googleapis.com/auth/contacts.readonly",
      "https://www.googleapis.com/auth/userinfo.email",
    ],
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
      logger.error("Error retrieving access token", e);
      throw e;
    });
};

interface SaveClientCredentialsResult {
  readonly userEmail: string;
}

interface SaveClientCredentialsParams {
  readonly tokenCode: string;
  readonly storage: SubscriberStorage;
}
const saveClientCredentialsForToken = async (
  params: SaveClientCredentialsParams
): Promise<SaveClientCredentialsResult> => {
  const { tokenCode, storage } = params;
  const credentials = await getOAuthClientCredentials(tokenCode);
  const oauthClient = getOauthClientForCredentials(credentials);

  if (credentials.access_token) {
    const tokenInfo = await oauthClient.getTokenInfo(credentials.access_token);

    if (tokenInfo.email) {
      await storage.saveSubscriber({
        emailAddress: tokenInfo.email,
        oauthCredentials: credentials,
      });
      return { userEmail: tokenInfo.email };
    } else {
      const error = new Error(
        "Unable to find to sub id for user in oauth credentials token info"
      );
      logger.error(error);
      throw error;
    }
  } else {
    const error = new Error(
      "Unable to find to find access token in oauth credentials"
    );
    logger.error(error);
    throw error;
  }
};

const getOauthClientForCredentials = (
  credentials: Credentials
): Oauth2Client.OAuth2Client => {
  const oauthClient = createOauthClient();
  oauthClient.setCredentials(credentials);
  return oauthClient;
};

export default {
  saveClientCredentialsForToken,
  getAuthUrl,
  getOauthClientForCredentials,
};
