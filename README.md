monorepo to handle notifying an email of their Google contacts birthday

## TODO

### App

- Write oauth credentials to file
- Setup daily cron to just print "hello world"
  - https://hackage.haskell.org/package/cron
- Create b-day notifier job to use oauth tokens, fetch birthdays, filter, then email
- Store oauth credentials in a DB
- nginx to front both API and frontend

### Dev

- Use .env for haskell
- Configure tsconfig more strictly
- Formatting
- Linting
- Tests
- Dockerfile
- CI/CD
- Strong Types?
