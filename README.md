monorepo to handle notifying an email of their Google contacts birthday

## TODO

### App

- Update oauth response to parse "id_token"
- Parse email from "id_token" JWT
- Make sure OauthRefresh interface can work with email
- Setup daily cron to just print "hello world"
  - https://hackage.haskell.org/package/cron
- Create b-day notifier job to use oauth tokens, fetch birthdays, filter, then email
- nginx to front both API and frontend

### Dev

- Configure tsconfig more strictly
- Formatting
- Linting
- Tests
- Dockerfile
- CI/CD
- Strong Types?
