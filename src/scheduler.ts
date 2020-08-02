import cron from "cron";

// everyday at 8 am "0 8 * * *",

const start = (sendBirthdayNotificationEmails: () => Promise<void>): void => {
  cron.job(
    "0 8 * * *",
    async function () {
      await sendBirthdayNotificationEmails();
    },
    null,
    true
  );
};

export default {
  start,
};
