monorepo to handle notifying an email of their Google contacts birthday

## TODO
* client -> frontend, api -> backend 
* convert frontend to react app
* get oauth working with react + api
  - will need to use servant-server for oauth callback
* get contacts via Google API
* store oauth data somehow? file first since the "job" process will need the data
* setup daily cron to just print "hello world"
  - https://hackage.haskell.org/package/cron
* send an email via send grid
* send an email based on birthday data
* create b-day notifier job to use oauth tokens, fetch birthdays, filter, then email
* nginx to front both API and frontend

* Linting
* Tests
* Dockerfile
* CI/CD
