This is a fun project to help me learn Haskell!

This should not be considered production level code, I am still a haskell newb
and the majority of the code is to "just make it work". As I get more experience
with Haskell I hope to improve this code, making things more idiomatic and
include proper tests.

monorepo to handle notifying an email of their Google contacts birthday

## Project Goal/Features:

This project spun up off of my own itch that I wanted to scratch. I keep track
of friend and family birthdays via their contact information stored in Google
Contacts. Unfortunately there does not seem to be a way to be notified of
birthdays that I can find.

The goal of this project is to enable the ability for a Google user to sign up
to be notified via a daily email about their Google Contact's birthdays.

## TODO

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
