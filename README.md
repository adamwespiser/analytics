# analytics

## Dev environment, MacOS Install Instructions
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
