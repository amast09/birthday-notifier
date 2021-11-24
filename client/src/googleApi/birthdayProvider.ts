import { people_v1 } from "googleapis/build/src/apis/people/v1";
import {
  chain,
  fold,
  fromNullable,
  map as mapOption,
  mapNullable,
  none,
  Option,
  some,
} from "fp-ts/lib/Option";
import { pipe } from "fp-ts/lib/pipeable";
import { DateTime } from "luxon";
import { GaxiosResponse } from "gaxios";
import BirthdayProvider, { NameWithBirthday } from "../types/BirthdayProvider";
import { Subscriber } from "../types/SubscriberStorage";
import logger from "../logger";

const GOOGLE_PEOPLE_CONNECTIONS_LIST_OPTIONS = Object.freeze({
  resourceName: "people/me",
  pageSize: 2000,
  personFields: "names,emailAddresses,birthdays",
});

const EMPTY_NAME_WITH_BDAY_ARRAY: NameWithBirthday[] = [];
const EMPTY_PERSON_SCHEMA_ARRAY: people_v1.Schema$Person[] = [];

const googleDateToNativeDate = (
  googleDate: people_v1.Schema$Date
): Option<DateTime> =>
  googleDate.month && googleDate.day
    ? some(
        DateTime.fromObject({
          year: googleDate.year || 1991,
          month: googleDate.month,
          day: googleDate.day,
          hour: 0,
          minute: 0,
          second: 0,
        })
      )
    : none;

const getBirthdayFromPerson = (
  person: people_v1.Schema$Person
): Option<DateTime> =>
  pipe(
    fromNullable(person.birthdays),
    mapNullable((birthdays) => birthdays[0]),
    mapNullable((birthday) => birthday.date),
    chain(googleDateToNativeDate)
  );

const getNameFromPerson = (person: people_v1.Schema$Person): Option<string> =>
  pipe(
    fromNullable(person.names),
    mapNullable((names) => names[0]),
    mapNullable((name) => name.displayName)
  );

const birthdayProvider = (
  getPeopleApiForSubscriber: (subscriber: Subscriber) => people_v1.People
): BirthdayProvider => ({
  getBirthdaysForSubscriber: (subscriber: Subscriber) =>
    getPeopleApiForSubscriber(subscriber)
      .people.connections.list(GOOGLE_PEOPLE_CONNECTIONS_LIST_OPTIONS)
      .then<NameWithBirthday[]>(
        (res: GaxiosResponse<people_v1.Schema$ListConnectionsResponse>) => {
          const connections: people_v1.Schema$Person[] =
            res.data.connections || EMPTY_PERSON_SCHEMA_ARRAY;

          return connections.reduce<NameWithBirthday[]>(
            (acc: NameWithBirthday[], person: people_v1.Schema$Person) => {
              const maybeBirthday = getBirthdayFromPerson(person);
              const maybeName = getNameFromPerson(person);

              const maybeNameWithBirthday: Option<NameWithBirthday> = chain(
                (name: string) =>
                  mapOption((birthday: DateTime) => ({ name, birthday }))(
                    maybeBirthday
                  )
              )(maybeName);

              return fold(
                () => acc,
                (nameWithBirthday: NameWithBirthday) =>
                  acc.concat(nameWithBirthday)
              )(maybeNameWithBirthday);
            },
            EMPTY_NAME_WITH_BDAY_ARRAY
          );
        }
      )
      .catch((e) => {
        logger.error(
          `Failed to list connections for ${subscriber.emailAddress}`
        );
        logger.error(e);
        throw e;
      }),
});

export default birthdayProvider;
