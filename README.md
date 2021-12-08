monorepo to handle notifying an email of their Google contacts birthday

## TODO

### App

- Store oauth credentials in a DB
- Setup daily cron to just print "hello world"
  - https://hackage.haskell.org/package/cron
- Create b-day notifier job to use oauth tokens, fetch birthdays, filter, then email
- nginx to front both API and frontend

### Dev

- Use .env for haskell
- Strong Types
- Configure tsconfig more strictly
- Formatting
- Linting
- Tests
- Dockerfile
- CI/CD
