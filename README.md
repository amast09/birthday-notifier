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

- Figure out "safe" HTTP operations
- Figure out "safe" postgres operations
- General clean-up to make things not haphazard
- Figure out "safe" ENV access
- Any other remaing TODO's
- verify Google Oauth JWT explicitly (https://developers.google.com/identity/protocols/oauth2/openid-connect#validatinganidtoken)
- k8s implementation

### Dev

- Configure tsconfig more strictly
- Formatting
- Linting
- Tests
- Better logging
- CI/CD
- Strong Types?
