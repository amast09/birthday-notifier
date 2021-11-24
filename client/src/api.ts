import Application from "koa";
import Koa from "koa";
import Router from "@koa/router";
import googleOauthClient from "./googleApi/oauthClient";
import HTTP_STATUS from "http-status-codes";
import inMemorySubscriberStorage from "./inMemorySubscriberStorage";
import logger from "./logger";
import { DateTime, Interval } from "luxon";

const api = new Koa();
const router = new Router();

api.use(async (ctx: Application.ExtendableContext, next) => {
  const start = DateTime.fromJSDate(new Date());
  await next();
  const end = DateTime.fromJSDate(new Date());
  const reqDuration = Interval.fromDateTimes(start, end).length("milliseconds");

  let logLevel = "info";
  if (ctx.status >= 500) {
    logLevel = "error";
  }
  if (ctx.status >= 400) {
    logLevel = "warn";
  }

  const msg = `${ctx.method} ${ctx.originalUrl} ${ctx.status} ${reqDuration}ms`;

  logger.log(logLevel, msg);
});

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

api.use(router.routes()).use(router.allowedMethods());

export default api;
