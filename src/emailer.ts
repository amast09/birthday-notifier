import sendGridMailer from "@sendgrid/mail";
import logger from "./logger";
import { NameWithBirthday } from "./types/BirthdayProvider";
import ENVIRONMENT, { EnvironmentKey } from "./environment";

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
        `${nameAndBirthday.name} ${nameAndBirthday.birthday.toFormat(
          "MM/D/YYYY"
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
    logger.error("failed to send birthday notification email", email, error);
  }
};

const emailer: Emailer = { sendBirthdayNotificationEmail }

export default emailer;
