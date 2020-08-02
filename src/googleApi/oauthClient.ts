import Oauth2Client from "googleapis-common";
import { google } from "googleapis";
import { Credentials } from "google-auth-library/build/src/auth/credentials";
import logger from "../logger";
import OauthStorage from "../types/Storage";

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
    scope: ["https://www.googleapis.com/auth/contacts.readonly", "openid"],
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
  readonly userId: string;
}

interface SaveClientCredentialsParams {
  readonly tokenCode: string;
  readonly storage: OauthStorage;
}
const saveClientCredentialsForToken = async (
  params: SaveClientCredentialsParams
): Promise<SaveClientCredentialsResult> => {
  const { tokenCode, storage } = params;
  const credentials = await getOAuthClientCredentials(tokenCode);
  const oauthClient = getOauthClientForCredentials(credentials);

  if (credentials.access_token) {
    const tokenInfo = await oauthClient.getTokenInfo(credentials.access_token);

    if (tokenInfo.sub) {
      await storage.saveOauthCredentials(tokenInfo.sub, credentials);
      return { userId: tokenInfo.sub };
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

interface GetAllOauthCredentialsParams {
  readonly storage: OauthStorage;
}

const getAllOauthClients = async (
  params: GetAllOauthCredentialsParams
): Promise<Oauth2Client.OAuth2Client[]> => {
  const credentials = await params.storage.getOauthCredentials();
  return Promise.all(
    credentials.map(getOauthClientForCredentials)
  );
};

export default {
  saveClientCredentialsForToken,
  getAuthUrl,
  getAllOauthClients,
};
