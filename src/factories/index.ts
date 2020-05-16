// factories/index.ts
import { register } from "fishery";
import Contact from "./Contact.factory";

const factories = register({
  Contact,
});

export default factories;
