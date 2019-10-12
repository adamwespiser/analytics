#!/bin/bash

# This is a MacOS only build script, for development environment
# requires stack, brew

## What are the steps we need here?
# 0. Load in environment variables to specify DB and target locations
# 1. build the stack project
# 2. set up a fresh instance for the database, run migrations
# 3. deploy database
# 4. deploy API server

# 0. Load in env variables
source config/dev.env

# 1. build the stack project
# this makes analytics-exe
stack build analytics:analytics-exe
stack build analytics:analytics-migrations

# 2. set up a fresh instance for the databse, run migrations
# Ensure postgres exists and is running
command -v postgres || brew install postgres
pg_ctl status -D /usr/local/var/postgres 2>&1 || pg_ctl -D /usr/local/var/postgres start
brew services list | grep postgresql || brew services start postgresql

# Destructively drop analytics and
dropdb $PGDB
createdb $PGDB -w # -w "no password"
createuser $PGUSER -d # -d "create db"

# run migrations
source config/dev.env && stack exec -- analytics-migrations


# 3. Deploy DB
echo "db available at $DBCONN"

# 4. Deploy API server
source config/dev.conf && stack exec -- analytics-exe


