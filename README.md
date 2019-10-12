# analytics

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
- [ ] Devops code for deploy. Investigate Docker, NixOs, and Heroku. QUESTION: what's the best reproducible, cross-platform build system for end users?
- [ ] Javascript Object for session with pageview/event methods, backed up by cookie store
- [ ] Test environmental variables + DB setup
- [ ] Test suite for [Servant Endpoints](https://docs.servant.dev/en/stable/cookbook/testing/Testing.html)
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
For the analytics server...
1. Set up heroku project
2. use the haskell build pack `$ heroku buildpacks:set https://github.com/mfine/heroku-buildpack-stack --app {project_name}`
3. In heroku, set environment variables, API_KEY, CORS_ORIGIN, DBCONN



