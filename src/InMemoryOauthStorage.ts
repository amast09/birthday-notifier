import OauthStorage from "./types/Storage";
import { Credentials } from "google-auth-library/build/src/auth/credentials";

const IN_MEMORY_DB: Record<string, Credentials> = {};

const InMemoryOauthStorage: OauthStorage = {
  getOauthCredentials: () =>
    new Promise((resolve) => resolve(Object.values(IN_MEMORY_DB))),
  saveOauthCredentials: (id: string, credentials: Credentials) =>
    new Promise((resolve) => {
      IN_MEMORY_DB[id] = credentials;
      resolve();
    }),
};

export default InMemoryOauthStorage;
