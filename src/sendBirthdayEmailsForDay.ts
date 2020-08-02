import { Emailer } from "./emailer";
import googleOauthClient from "./googleApi/oauthClient";
import { google } from "googleapis";
import GoogleContactProvider from "./googleApi/birthdayProvider";
import SubscriberStorage from "./types/SubscriberStorage";

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
        START HERE filter to birthdays for today

        const birthdays = await GoogleContactProvider(peopleApi)();
      return emailer.sendBirthdayNotificationEmail({
        emailAddress: subscriber.emailAddress,
        nameWithBirthdays: birthdays,
      });
    })
  ).then(() => {});
};

export default sendBirthdayEmailsForDay;
