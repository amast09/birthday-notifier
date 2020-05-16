import { Factory } from "fishery";
import Contact from "../types/Contact";
import { DateTime } from "luxon";
import * as faker from "faker";

export default Factory.define<Contact>(() => ({
  name: faker.fake("{{name.firstName}} {{name.lastName}}"),
  birthday: DateTime.utc(),
}));
