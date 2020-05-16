import Application from "koa";

const Koa = require("koa");
const app = new Koa();

app.use(async (ctx: Application.BaseContext) => {
  ctx.body = "Hello World";
});

app.listen(3000);
