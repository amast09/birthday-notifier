import cron from "cron";
import logger from "./logger";

const start = (sendBirthdayNotificationEmails: () => Promise<void>): void => {
  cron.job(
    "0 0 * * *",
    async function () {
      logger.info("Kicking off Email Job");
      await sendBirthdayNotificationEmails();
    },
    () => {
      logger.info("Email Job Complete");
    },
    true
  );
};

export default {
  start,
};
