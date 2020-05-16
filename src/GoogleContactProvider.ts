import ContactProvider from "./types/ContactProvider";
import { of } from "fp-ts/lib/Task";
import Contact from "./types/Contact";
import { GoogleApis } from "googleapis/build/src/googleapis";

const GoogleContactProvider = (googleApi: GoogleApis): ContactProvider => ({
  getContacts: () => {
    googleApi.people("v1").people.getBatchGet();
    return of<Contact[]>([]);
  },
});

export default GoogleContactProvider;
