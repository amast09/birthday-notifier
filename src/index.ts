import ENVIRONMENT, { EnvironmentKey } from "./environment";
import api from "./api";
import scheduler from "./scheduler";
import sendBirthdayEmailsForDay from "./sendBirthdayEmailsForDay";
import emailer from "./emailer";
import inMemorySubscriberStorage from "./inMemorySubscriberStorage";

scheduler.start(async () => {
  await sendBirthdayEmailsForDay({
    emailer,
    subscriberStorage: inMemorySubscriberStorage,
  });
});

api.listen(ENVIRONMENT[EnvironmentKey.API_PORT]);
