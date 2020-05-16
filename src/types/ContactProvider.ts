import Contact from "./Contact";
import { Task } from "fp-ts/lib/Task";

interface ContactProvider {
  readonly getContacts: () => Task<Contact[]>;
}

export default ContactProvider;
