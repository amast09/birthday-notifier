import { DateTime } from "luxon";
import { Subscriber } from "./SubscriberStorage";

export interface NameWithBirthday {
  readonly name: string;
  readonly birthday: DateTime;
}

interface BirthdayProvider {
  readonly getBirthdaysForSubscriber: (
    subscriber: Subscriber
  ) => Promise<NameWithBirthday[]>;
}

export default BirthdayProvider;
