# analytics
[![Build Status](https://travis-ci.org/adamwespiser/analytics.svg?branch=master)](https://travis-ci.org/adamwespiser/analytics)


## Philosophy
Data helps us make better decisions, and the tools that enable this should be easy to understand, transparent, and fully controllable.

##  About
The aim of this project is to provide a lightweight, self-deployable, alternative to Google Analytics to support fast data driven decision making in a web development context.
It's designed to give engineering teams an easy way to record events, pageviews, and conduct downstream analysis, like attribution modelling, A/B or MAB testing, session tracking, et cetera.
All of which are possible in a ready to deploy, OSS system where you have access over the data!

Currently, this project is a work in progress, and the features currently complete are:
 - Generate UUID representing User Session and store in PostgreSQL
 - Store Page Views in PostgreSQL
 - Store Events in PostgreSQL
 - Protect Routes via API KEY
 - Javascript code to generate session
 - SQL code to intialize DB

### TODO
- [x] Devops code for build -> TravisCI integration w/ mock test suites
- [x] Test environmental variables + DB setup
- [x] Test suite for [Servant Endpoints](https://docs.servant.dev/en/stable/cookbook/testing/Testing.html)
- [ ] Javascript Object for session with pageview/event methods, backed up by cookie store
- [ ] Test suite for Javascript Integration (create user session, navigate through pages, check db)
- [ ] Support for A/B test and MAB variant assignment
- [ ] Add top-level domain field to pageview
- [ ] Send more tracking data from browser back to db (user agent, time zone, language, ip, et cetera)
- [ ] Heroku Deploy for pre-existing DB
- [x] DB deploy script [POSIX compliant systems]


## Install: Dev environment, MacOS Install Instructions
#### Set up Postgres
First, you need to have postgres installed and running locally. To do this, we will use the postgresql helper fns pg_ctl, create{db,user}
```
$ brew install postgresql
$ pg_ctl -D /usr/local/var/postgres start && brew services start postgresql
$ createdb analytics
$ createuser analytics
```
#### Run Migrations
*Note: these are destructive, table destroying actions, use them only to set up the the database*
```
$ stack build
$ source config/dev.env && stack exec -- analytics-migrations
```

#### Heroku Deploy
For the analytics server.
1. Make sure a production DB is set up, and that the migrations/db set up have been successfully performed. This can be done by getting the prod config, and running `source [production config] && stack exec -- analytics-migrations`.
2. Set up heroku project
3. use the haskell build pack `$ heroku buildpacks:set https://github.com/mfine/heroku-buildpack-stack --app {project_name}`
4. In heroku, set environment variables, API_KEY, CORS_ORIGIN, DBCONN. Note, on heroku, the environmental variable `PORT` has to be set by the heroku environment.
5. on cmd line, `$ heroku git:remote -a [project-name]`

#### Local Test Env
The local tests will spin up a postgres instance, and a warp server for the sake of testing. This is designed to mock the TravisCI environment. To run the tests with the proper configuration variables set:    
```
$ source config/test-local.env && stack test
```

