import Application from "koa";
import Koa from "koa";
import Router from "@koa/router";
import googleOauthClient from "./googleApi/oauthClient";
import HTTP_STATUS from "http-status-codes";
import inMemorySubscriberStorage from "./inMemorySubscriberStorage";
import GoogleContactProvider from "./googleApi/birthdayProvider";
import { google } from "googleapis";
import ENVIRONMENT, {EnvironmentKey} from "./environment";

const app = new Koa();
const router = new Router();

router.get("/subscribe", async (ctx: Application.BaseContext) => {
  const googleAuthUrl = googleOauthClient.getAuthUrl();
  ctx.redirect(googleAuthUrl);
});

router.get(
  "/google-oauth-callback",
  async (ctx: Application.ExtendableContext) => {
    try {
      await googleOauthClient.saveClientCredentialsForToken({
        storage: inMemorySubscriberStorage,
        tokenCode: ctx.query.code,
      });
      ctx.response.status = HTTP_STATUS.NO_CONTENT;
    } catch (_) {
      ctx.response.status = HTTP_STATUS.FAILED_DEPENDENCY;
    }
  }
);

router.get("/birthdays", async (ctx: Application.BaseContext) => {
  const subscribers = await inMemorySubscriberStorage.getSubscribers();
  const allBirthdays = await Promise.all(
    subscribers.map(async (subscriber) => {
      const oauthClient = googleOauthClient.getOauthClientForCredentials(
        subscriber.oauthCredentials
      );
      const peopleApi = google.people({ version: "v1", auth: oauthClient });
      const birthdays = await GoogleContactProvider(peopleApi)();
      // await emailer.sendBirthdayNotificationEmail({
      //   emailAddress: subscriber.emailAddress,
      //   nameWithBirthdays: birthdays,
      // });
      return { [subscriber.emailAddress]: birthdays };
    })
  );

  ctx.body = { subscribers: allBirthdays };
});

app.use(router.routes()).use(router.allowedMethods());

app.listen(ENVIRONMENT[EnvironmentKey.API_PORT]);
