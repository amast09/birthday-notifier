import cron from "cron";
import logger from "./logger";
import sendBirthdayEmailsForBirthDate from "./jobs/sendBirthdayEmailsForBirthDate";
import emailer from "./emailer";
import inMemorySubscriberStorage from "./inMemorySubscriberStorage";
import { DateTime } from "luxon";
import birthdayProvider from "./googleApi/birthdayProvider";
import oauthClient from "./googleApi/oauthClient";

const start = (): void => {
  cron.job(
    "0 0 * * *",
    async function () {
      const today = DateTime.fromJSDate(new Date()).startOf("day");
      logger.info(
        `Email Job for ${today.toLocaleString(DateTime.DATE_HUGE)} Started`
      );
      await sendBirthdayEmailsForBirthDate({
        emailer,
        subscriberStorage: inMemorySubscriberStorage,
        birthDate: today,
        birthdayProvider: birthdayProvider(
          oauthClient.getPeopleApiForSubscriber
        ),
      });
    },
    () => {
      const today = DateTime.fromJSDate(new Date()).startOf("day");
      logger.info(
        `Email Job for ${today.toLocaleString(DateTime.DATE_HUGE)} Complete`
      );
    },
    true
  );
};

export default {
  start,
};
