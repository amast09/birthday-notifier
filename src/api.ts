import Application from "koa";
import { google } from "googleapis";
import dotenv from "dotenv";
import * as readline from "readline";
import Oauth2Client from "googleapis-common";
import { TaskEither, tryCatch, map } from "fp-ts/lib/TaskEither";
import Koa from "koa";
import Router from "@koa/router";

const getOAuth2Client = (): TaskEither<string, Oauth2Client.OAuth2Client> => {
  const SCOPES = ["https://www.googleapis.com/auth/contacts.readonly"];
  const oAuth2Client = new google.auth.OAuth2(
    process.env.GOOGLE_PEOPLE_API_CLIENT_ID,
    process.env.GOOGLE_PEOPLE_API_CLIENT_SECRET,
    process.env.GOOGLE_PEOPLE_API_REDIRECT_URL
  );
  const authUrl = oAuth2Client.generateAuthUrl({
    access_type: "offline",
    scope: SCOPES,
  });

  console.log("Authorize this app by visiting this url:", authUrl);
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
  });

  const promise = new Promise<Oauth2Client.OAuth2Client>((resolve, reject) => {
    rl.question("Enter the code from that page here: ", (code) => {
      rl.close();

      oAuth2Client.getToken(code, (err, token) => {
        if (err || !token) {
          reject(err);
          console.error("Error retrieving access token", err);
        } else {
          oAuth2Client.setCredentials(token);
          resolve(oAuth2Client);
        }
      });
    });
  });

  return tryCatch(() => promise, String);
};

if (process.env.NODE_ENV !== "production") {
  dotenv.config();
}

const x = map((client: Oauth2Client.OAuth2Client) => {
  const service = google.people({ version: "v1", auth: client });

  service.people.connections.list(
    {
      resourceName: "people/me",
      pageSize: 10,
      personFields: "names,emailAddresses",
    },
    (err, res) => {
      if (err) return console.error("The API returned an error: " + err);
      if (res && res.data.connections) {
        const connections = res.data.connections;
        console.log("Connections:");
        connections.forEach((person) => {
          if (person.names && person.names.length > 0) {
            console.log(person.names[0].displayName);
          } else {
            console.log("No display name found for connection.");
          }
        });
      }
    }
  );
})(getOAuth2Client());

x();

const app = new Koa();
const router = new Router();

router.get("/google-oauth-callback", async (ctx: Application.BaseContext) => {
  ctx.body = { code: ctx.query.code };
});

app.use(router.routes()).use(router.allowedMethods());

app.listen(process.env.API_PORT);
