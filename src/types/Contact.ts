import { DateTime } from "luxon";

interface Contact {
  readonly name: string;
  readonly birthday: DateTime;
}

export default Contact;
