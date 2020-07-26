import { Credentials } from "google-auth-library/build/src/auth/credentials";

interface OauthStorage {
  readonly saveOauthCredentials: (
    id: string,
    credentials: Credentials
  ) => Promise<void>;
  readonly getOauthCredentials: () => Promise<Credentials[]>;
}

export default OauthStorage;
