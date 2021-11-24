import cron from "cron";
import logger from "./logger";
import sendBirthdayEmailsForBirthDate from "./jobs/sendBirthdayEmailsForBirthDate";
import emailer from "./emailer";
import inMemorySubscriberStorage from "./inMemorySubscriberStorage";
import { DateTime } from "luxon";
import birthdayProvider from "./googleApi/birthdayProvider";
import oauthClient from "./googleApi/oauthClient";

/*
{"level":"error","timestamp":"2020-08-05 08:00:00","message":"No refresh token is set.","stack":"Error: No refresh token is set.\n    at OAuth2Client.refreshTokenNoCache (/home/amast/repos/birthday-notifier-server/node_modules/google-auth-library/build/src/auth/oauth2client.js:161:19)\n    at OAuth2Client.refreshToken (/home/amast/repos/birthday-notifier-server/node_modules/google-auth-library/build/src/auth/oauth2client.js:142:25)\n    at OAuth2Client.getRequestMetadataAsync (/home/amast/repos/birthday-notifier-server/node_modules/google-auth-library/build/src/auth/oauth2client.js:256:28)\n    at OAuth2Client.requestAsync (/home/amast/repos/birthday-notifier-server/node_modules/google-auth-library/build/src/auth/oauth2client.js:329:34)\n    at OAuth2Client.request (/home/amast/repos/birthday-notifier-server/node_modules/google-auth-library/build/src/auth/oauth2client.js:323:25)\n    at createAPIRequestAsync (/home/amast/repos/birthday-notifier-server/node_modules/googleapis-common/build/src/apirequest.js:291:27)\n    at Object.createAPIRequest (/home/amast/repos/birthday-notifier-server/node_modules/googleapis-common/build/src/apirequest.js:45:16)\n    at Resource$People$Connections.list (/home/amast/repos/birthday-notifier-server/node_modules/googleapis/build/src/apis/people/v1.js:536:44)\n    at Object.getBirthdaysForSubscriber (/home/amast/repos/birthday-notifier-server/dist/googleApi/birthdayProvider.js:31:29)\n    at /home/amast/repos/birthday-notifier-server/dist/jobs/sendBirthdayEmailsForBirthDate.js:7:50"}
(node:10000) UnhandledPromiseRejectionWarning: Error: No refresh token is set.
    at OAuth2Client.refreshTokenNoCache (/home/amast/repos/birthday-notifier-server/node_modules/google-auth-library/build/src/auth/oauth2client.js:161:19)
    at OAuth2Client.refreshToken (/home/amast/repos/birthday-notifier-server/node_modules/google-auth-library/build/src/auth/oauth2client.js:142:25)
    at OAuth2Client.getRequestMetadataAsync (/home/amast/repos/birthday-notifier-server/node_modules/google-auth-library/build/src/auth/oauth2client.js:256:28)
    at OAuth2Client.requestAsync (/home/amast/repos/birthday-notifier-server/node_modules/google-auth-library/build/src/auth/oauth2client.js:329:34)
    at OAuth2Client.request (/home/amast/repos/birthday-notifier-server/node_modules/google-auth-library/build/src/auth/oauth2client.js:323:25)
    at createAPIRequestAsync (/home/amast/repos/birthday-notifier-server/node_modules/googleapis-common/build/src/apirequest.js:291:27)
    at Object.createAPIRequest (/home/amast/repos/birthday-notifier-server/node_modules/googleapis-common/build/src/apirequest.js:45:16)
    at Resource$People$Connections.list (/home/amast/repos/birthday-notifier-server/node_modules/googleapis/build/src/apis/people/v1.js:536:44)
    at Object.getBirthdaysForSubscriber (/home/amast/repos/birthday-notifier-server/dist/googleApi/birthdayProvider.js:31:29)
    at /home/amast/repos/birthday-notifier-server/dist/jobs/sendBirthdayEmailsForBirthDate.js:7:50
(node:10000) UnhandledPromiseRejectionWarning: Unhandled promise rejection. This error originated either by throwing inside of an async function without a catch block, or by rejecting a promise which was not handled with .catch(). To terminate the node process on unhandled promise rejection, use the CLI flag `--unhandled-rejections=strict` (see https://nodejs.org/api/cli.html#cli_unhandled_rejections_mode). (rejection id: 4)
hkjkhjkhjkjkhjkkhjkjhjkkhjhjkhjkhjkhjkhjhjkhjkhjkhjkhjkhjkhjkhjkhjkhjkhjkhjkhjkhjkhjjkhjhkjhjkhkjhjkhjkhjkhjkkhjkhj
 */

const start = (): void => {
  cron.job(
    "0 8 * * *",
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
