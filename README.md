monorepo to handle notifying an email of their Google contacts birthday

## TODO

### App

- Update `AccessTokensResponse` to parse "id_token" JWT string
- Add function to `AccessTokensResponse` to parse email from "id_token" JWT
- Make sure OauthRefresh interface can work with email
- Update oauth callback to store email + refresh token
- Setup daily cron to just print "hello world"
  - https://hackage.haskell.org/package/cron
- Create b-day notifier job to use oauth tokens, fetch birthdays, filter, then email
- nginx to front both API and frontend
- verify Google Oauth JWT explicitly (https://developers.google.com/identity/protocols/oauth2/openid-connect#validatinganidtoken)

### Dev

- Configure tsconfig more strictly
- Formatting
- Linting
- Tests
- Dockerfile
- CI/CD
- Strong Types?
