import { Emailer } from "./emailer";
import googleOauthClient from "./googleApi/oauthClient";
import { google } from "googleapis";
import GoogleContactProvider from "./googleApi/birthdayProvider";
import SubscriberStorage from "./types/SubscriberStorage";
import { DateTime } from "luxon";

interface SendBirthdayEmailsForDayParams {
  readonly emailer: Emailer;
  readonly subscriberStorage: SubscriberStorage;
}

const sendBirthdayEmailsForDay = async (
  params: SendBirthdayEmailsForDayParams
): Promise<void> => {
  const { emailer, subscriberStorage } = params;
  const subscribers = await subscriberStorage.getSubscribers();

  Promise.all(
    subscribers.map(async (subscriber) => {
      const oauthClient = googleOauthClient.getOauthClientForCredentials(
        subscriber.oauthCredentials
      );
      const peopleApi = google.people({ version: "v1", auth: oauthClient });

      const today = DateTime.fromJSDate(new Date()).startOf("day");
      const birthdays = await GoogleContactProvider(peopleApi)();
      const birthdaysForDay = birthdays.filter(({ birthday }) =>
        birthday.month === today.month && birthday.day === today.day
      );

      if (birthdaysForDay.length > 0) {
        return emailer.sendBirthdayNotificationEmail({
          emailAddress: subscriber.emailAddress,
          nameWithBirthdays: birthdaysForDay,
        });
      } else {
        return Promise.resolve();
      }
    })
  ).then(() => {});
};

export default sendBirthdayEmailsForDay;
