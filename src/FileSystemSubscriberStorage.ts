import SubscriberStorage from "./types/SubscriberStorage";
import { Credentials } from "google-auth-library/build/src/auth/credentials";
import defaultFs from "fs";
import logger from "./logger";

const fs = defaultFs.promises;

type DB_TABLE_TYPE = Record<string, Credentials>;
const DB_FILE_NAME = "birthday-notifier-db.json";
const EMPTY_DB: DB_TABLE_TYPE = {};

const getDb = async (): Promise<DB_TABLE_TYPE> => {
  let subscribersAsJson = EMPTY_DB;

  try {
    const bufferOfJsonFileData = await fs.readFile(DB_FILE_NAME);
    subscribersAsJson = JSON.parse(
      bufferOfJsonFileData.toString()
    ) as DB_TABLE_TYPE;
  } catch (_) {
    logger.info("failed to parse file of JSON");
  }

  return subscribersAsJson;
};

// const saveDb = async (newDbContents: DB_TABLE_TYPE): Promise<void> => {
//
// }

const FileSystemSubscriberStorage: SubscriberStorage = {
  getSubscribers: async () => {
    const db = await getDb();

    return Object.entries(db).map(([key, value]) => ({
      emailAddress: key,
      oauthCredentials: value,
    }));
  },
  saveSubscriber: ({ emailAddress, oauthCredentials }) =>
    new Promise((resolve) => {
      console.log(emailAddress);
      console.log(oauthCredentials);
      resolve();
    }),
};

export default FileSystemSubscriberStorage;
