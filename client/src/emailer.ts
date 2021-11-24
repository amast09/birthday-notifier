import sendGridMailer from "@sendgrid/mail";
import logger from "./logger";
import { NameWithBirthday } from "./types/BirthdayProvider";
import ENVIRONMENT, { EnvironmentKey } from "./environment";
import { DateTime } from "luxon";

const BIRTHDAY_NOTIFICATION_SUBJECT = "Birthday Notifications for the Day";

sendGridMailer.setApiKey(ENVIRONMENT[EnvironmentKey.SEND_GRID_API_KEY]);

export interface Emailer {
  readonly sendBirthdayNotificationEmail: (
    params: BirthdayNotificationEmailParams
  ) => Promise<void>;
}

interface BirthdayNotificationEmailParams {
  readonly emailAddress: string;
  readonly nameWithBirthdays: NameWithBirthday[];
}

const sendBirthdayNotificationEmail = async (
  params: BirthdayNotificationEmailParams
): Promise<void> => {
  const birthdayTextList = params.nameWithBirthdays
    .map(
      (nameAndBirthday) =>
        `${nameAndBirthday.name} ${nameAndBirthday.birthday.toLocaleString(
          DateTime.DATETIME_HUGE_WITH_SECONDS
        )}`
    )
    .join("\n");

  const email = {
    to: params.emailAddress,
    from: ENVIRONMENT[EnvironmentKey.FROM_EMAIL_ADDRESS],
    subject: BIRTHDAY_NOTIFICATION_SUBJECT,
    text: birthdayTextList,
  };

  try {
    await sendGridMailer.send(email);
  } catch (error) {
    logger.error("Failed to send birthday notification email");
    logger.error(email);
    logger.error(error);
  }
};

const emailer: Emailer = { sendBirthdayNotificationEmail };

export default emailer;
