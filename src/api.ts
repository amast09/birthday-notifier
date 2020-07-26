import Application from "koa";
import dotenv from "dotenv";
import Koa from "koa";
import Router from "@koa/router";
import googleOauthClient from "./googleApi/oauthClient";
import { google } from "googleapis";

if (process.env.NODE_ENV !== "production") {
  dotenv.config();
}

const app = new Koa();
const router = new Router();

router.get("/", async (ctx: Application.BaseContext) => {
  const googleAuthUrl = googleOauthClient.getAuthUrl();
  ctx.redirect(googleAuthUrl);
});

router.get("/google-oauth-callback", async (ctx: Application.BaseContext) => {
  const credentials = await googleOauthClient.getOAuthClientCredentials(
    ctx.query.code
  );
  const oauthClient = googleOauthClient.getOauthClientForCredentials(
    credentials
  );
  const gmailApi = google.gmail({ version: "v1", auth: oauthClient });
  const xyz = await gmailApi.users.getProfile({ userId: "me" });
  // const peopleApi = google.people({ version: "v1", auth: oauthClient });
  // const contactProvider = GoogleContactProvider(peopleApi);
  // const contactsWithBirthdays = await contactProvider.getContacts();
  ctx.body = { contacts: xyz.data };
});

router.get("/birthdays", async (ctx: Application.BaseContext) => {
  ctx.body = { contacts: [] };
});

app.use(router.routes()).use(router.allowedMethods());

app.listen(process.env.API_PORT);
