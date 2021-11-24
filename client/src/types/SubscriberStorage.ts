import { Credentials } from "google-auth-library/build/src/auth/credentials";

export interface Subscriber {
  readonly emailAddress: string;
  readonly oauthCredentials: Credentials;
}

interface SubscriberStorage {
  readonly saveSubscriber: (params: Subscriber) => Promise<void>;
  readonly getSubscribers: () => Promise<Subscriber[]>;
}

export default SubscriberStorage;
