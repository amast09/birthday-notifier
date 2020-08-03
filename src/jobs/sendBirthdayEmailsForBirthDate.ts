import { Emailer } from "../emailer";
import SubscriberStorage from "../types/SubscriberStorage";
import { DateTime } from "luxon";
import BirthdayProvider from "../types/BirthdayProvider";

interface Params {
  readonly emailer: Emailer;
  readonly subscriberStorage: SubscriberStorage;
  readonly birthdayProvider: BirthdayProvider;
  readonly birthDate: DateTime;
}

const sendBirthdayEmailsForBirthDate = async (
  params: Params
): Promise<void> => {
  const { emailer, subscriberStorage, birthdayProvider, birthDate } = params;

  const subscribers = await subscriberStorage.getSubscribers();

  Promise.all(
    subscribers.map(async (subscriber) => {
      const birthdays = await birthdayProvider.getBirthdaysForSubscriber(
        subscriber
      );
      const birthdaysForDay = birthdays.filter(
        ({ birthday }) =>
          birthday.month === birthDate.month && birthday.day === birthDate.day
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

export default sendBirthdayEmailsForBirthDate;
