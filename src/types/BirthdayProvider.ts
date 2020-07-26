import { DateTime } from "luxon";

export interface NameWithBirthday {
  readonly name: string;
  readonly birthday: DateTime;
}

type BirthdayProvider = () => Promise<NameWithBirthday[]>;

export default BirthdayProvider;
