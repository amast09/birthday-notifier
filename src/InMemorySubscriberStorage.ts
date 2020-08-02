import SubscriberStorage from "./types/SubscriberStorage";
import { Credentials } from "google-auth-library/build/src/auth/credentials";

const IN_MEMORY_DB: Record<string, Credentials> = {};

const InMemorySubscriberStorage: SubscriberStorage = {
  getSubscribers: () =>
    new Promise((resolve) =>
      resolve(
        Object.entries(IN_MEMORY_DB).map(([key, value]) => ({
          emailAddress: key,
          oauthCredentials: value,
        }))
      )
    ),
  saveSubscriber: ({ emailAddress, oauthCredentials }) =>
    new Promise((resolve) => {
      IN_MEMORY_DB[emailAddress] = oauthCredentials;
      resolve();
    }),
};

export default InMemorySubscriberStorage;
