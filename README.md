monorepo to handle notifying an email of their Google contacts birthday

## TODO

### App

- Get Oauth working
  - Add API callback route (servant-server)
  - Make API call to get token + refresh token
  - Store data in DB
- Setup daily cron to just print "hello world"
  - https://hackage.haskell.org/package/cron
- Get contacts via Google API
- Send an email via send grid
- Send an email based on birthday data
- Create b-day notifier job to use oauth tokens, fetch birthdays, filter, then email
- nginx to front both API and frontend

### Dev

- Add haskell formatter
- Configure tsconfig more strictly
- Formatting
- Linting
- Tests
- Dockerfile
- CI/CD
