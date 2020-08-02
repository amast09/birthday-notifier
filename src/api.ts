import Application from "koa";
import dotenv from "dotenv";
import Koa from "koa";
import Router from "@koa/router";
import googleOauthClient from "./googleApi/oauthClient";
import HTTP_STATUS from "http-status-codes";
import InMemorySubscriberStorage from "./InMemorySubscriberStorage";
import GoogleContactProvider from "./googleApi/birthdayProvider";
import { google } from "googleapis";

if (process.env.NODE_ENV !== "production") {
  dotenv.config();
}

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
        storage: InMemorySubscriberStorage,
        tokenCode: ctx.query.code,
      });
      ctx.response.status = HTTP_STATUS.NO_CONTENT;
    } catch (_) {
      ctx.response.status = HTTP_STATUS.FAILED_DEPENDENCY;
    }
  }
);

router.get("/birthdays", async (ctx: Application.BaseContext) => {
  const subscribers = await InMemorySubscriberStorage.getSubscribers();
  const allBirthdays = await Promise.all(
    subscribers.map(async (subscriber) => {
      const oauthClient = googleOauthClient.getOauthClientForCredentials(
        subscriber.oauthCredentials
      );
      const peopleApi = google.people({ version: "v1", auth: oauthClient });
      const birthdays = await GoogleContactProvider(peopleApi)();
      return { [subscriber.emailAddress]: birthdays };
    })
  );

  ctx.body = { subscribers: allBirthdays };
});

app.use(router.routes()).use(router.allowedMethods());

app.listen(process.env.API_PORT);
