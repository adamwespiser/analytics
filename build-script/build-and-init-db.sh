#!/bin/bash

ABOUT <<aboutMessage
Usage "build-and-init-db.sh {dev,test,prod}.env}"
must have one argument, which is the environment
What are the steps we need here?
0. Load in environment variables to specify DB and target locations
1. build the stack project
2. set up a fresh instance for the database, run migrations
3. deploy database
aboutMessage

if [[ -e $1 ]] ; then
  echo -e "Loading from $1"
else
  echo -e "Usage "build-and-init-db.sh {dev,test,prod}.env}""
  exit 1
fi

stackpath=$(command -v stack)
if [[ -x $stackpath ]] ; then
  echo -e "Using stack @ $stackpath"
else
  echo -e "Cannot find stack, please download it"
  exit 1
fi

# requires stack, postgres
set -e

# 0. Load in env variables
source $1

# 1. build the stack project
stack build analytics:analytics-migrations

# 2. set up a fresh instance for the databse, run migrations
# Ensure postgres exists and is running
command -v postgres
pg_ctl status -D /usr/local/var/postgres 2>&1 || pg_ctl -D /usr/local/var/postgres start
echo -e "posgres is turned on"

# Destructively drop analytics and
dropdb $PGDB
createdb $PGDB -w # -w "no password"
createuser $PGUSER -d # -d "create db"
echo -e "$PGUSER set up on $PGDB"


# run migrations
source $1 && stack exec -- analytics-migrations
echo -e "migrations complete"

# 3. Deploy DB
echo -e "db available at $DBCONN"
