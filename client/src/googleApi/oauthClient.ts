import Oauth2Client from "googleapis-common";
import { google } from "googleapis";
import { Credentials } from "google-auth-library/build/src/auth/credentials";
import logger from "../logger";
import SubscriberStorage, { Subscriber } from "../types/SubscriberStorage";
import ENVIRONMENT, { EnvironmentKey } from "../environment";
import { people_v1 } from "googleapis/build/src/apis/people/v1";

const createOauthClient = () =>
  new google.auth.OAuth2(
    ENVIRONMENT[EnvironmentKey.GOOGLE_PEOPLE_API_CLIENT_ID],
    ENVIRONMENT[EnvironmentKey.GOOGLE_PEOPLE_API_CLIENT_SECRET],
    ENVIRONMENT[EnvironmentKey.GOOGLE_PEOPLE_API_REDIRECT_URL]
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

const getOauthClientForCredentials = (
    credentials: Credentials
): Oauth2Client.OAuth2Client => {
  const oauthClient = createOauthClient();
  oauthClient.setCredentials(credentials);
  return oauthClient;
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
        "Unable to get email in oauth credential's token info"
      );
      logger.error(error);
      throw error;
    }
  } else {
    const error = new Error(
      "Unable to get access token from oauth credentials"
    );
    logger.error(error);
    throw error;
  }
};

const getPeopleApiForSubscriber = (
  subscriber: Subscriber
): people_v1.People => {
  const oauthClient = getOauthClientForCredentials(subscriber.oauthCredentials);
  return google.people({ version: "v1", auth: oauthClient });
};

export default {
  saveClientCredentialsForToken,
  getAuthUrl,
  getPeopleApiForSubscriber,
};
