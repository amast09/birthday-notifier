import logger from "./logger";
import dotenv from "dotenv";
import makeIsEnumTypeGuard from "./utilities/isEnumTypeGuard";

export enum EnvironmentKey {
  API_PORT = "API_PORT",
  GOOGLE_PEOPLE_API_CLIENT_ID = "GOOGLE_PEOPLE_API_CLIENT_ID",
  GOOGLE_PEOPLE_API_CLIENT_SECRET = "GOOGLE_PEOPLE_API_CLIENT_SECRET",
  GOOGLE_PEOPLE_API_REDIRECT_URL = "GOOGLE_PEOPLE_API_REDIRECT_URL",
  SEND_GRID_API_KEY = "SEND_GRID_API_KEY",
  FROM_EMAIL_ADDRESS = "FROM_EMAIL_ADDRESS",
}

type Environment = Record<EnvironmentKey, string>;

if (process.env.NODE_ENV !== "production") {
  dotenv.config();
}

const isEnvironmentKeyEnum = makeIsEnumTypeGuard(EnvironmentKey);

const ENVIRONMENT: Environment = Object.values(EnvironmentKey)
  .filter(isEnvironmentKeyEnum)
  .reduce((acc: Environment, key: EnvironmentKey) => {
    const environmentValue = process.env[key];
    if (!environmentValue) {
      const error = new Error(`Missing Environment Variable ${key}`);
      logger.error(error);
      throw error;
    } else {
      acc[key] = environmentValue;
    }

    return acc;
  }, {} as Record<EnvironmentKey, string>);

export default ENVIRONMENT;
