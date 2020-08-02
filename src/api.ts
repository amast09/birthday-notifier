import Application from "koa";
import dotenv from "dotenv";
import Koa from "koa";
import Router from "@koa/router";
import googleOauthClient from "./googleApi/oauthClient";
import HTTP_STATUS from "http-status-codes";
import InMemoryOauthStorage from "./InMemoryOauthStorage";
import GoogleContactProvider from "./googleApi/birthdayProvider";
import { google } from "googleapis";
import { flatten } from "fp-ts/lib/Array";

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
    const saveCredentialsPromise = googleOauthClient.saveClientCredentialsForToken(
      {
        storage: InMemoryOauthStorage,
        tokenCode: ctx.query.code,
      }
    );

    saveCredentialsPromise
      .then(() => {
        ctx.response.status = HTTP_STATUS.NO_CONTENT;
      })
      .catch(() => {
        ctx.response.status = HTTP_STATUS.FAILED_DEPENDENCY;
      });
  }
);

router.get("/birthdays", async (ctx: Application.BaseContext) => {
  const oauthClients = await googleOauthClient.getAllOauthClients({
    storage: InMemoryOauthStorage,
  });
  const futureAllBirthdays = Promise.all(
    oauthClients
      .map((oauthClient) => google.people({ version: "v1", auth: oauthClient }))
      .map(GoogleContactProvider)
      .map((getBirthdays) => getBirthdays())
  );
  const allBirthdays = flatten(await futureAllBirthdays);
  ctx.body = { namesWithBirthdays: allBirthdays };
});

app.use(router.routes()).use(router.allowedMethods());

app.listen(process.env.API_PORT);
