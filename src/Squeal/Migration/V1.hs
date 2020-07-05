module Squeal.Migration.V1 where

import Squeal.PostgreSQL
import Squeal.Schema


initMigration :: Definition (Public '[]) DB
initMigration = createTable #events
  ( serial `as` #id :*
    (uuid & notNullable) `as` #user_session_id :*
    (text & notNullable) `as` #category :*
    (text & notNullable) `as` #text :*
    ((default_ (UnsafeExpression "current_timestamp") (notNullable timestamptz) `as` #modtime))
  )
  ( primaryKey #id `as` #pk_events ) >>>
  createTable #page_view
  ( serial `as` #id :*
    (uuid & notNullable) `as` #user_session_id :*
    (text & notNullable) `as` #url_filepath :*
    ((default_ (UnsafeExpression "current_timestamp") (notNullable timestamptz) `as` #modtime))
  )
  ( primaryKey #id `as` #pk_page_view ) >>>
  createTable #user_session
  ((default_ (UnsafeExpression "md5(random()::text || clock_timestamp()::text)::uuid") (notNullable uuid) `as` #id) :*
    ((default_ (UnsafeExpression "current_timestamp") (notNullable timestamptz) `as` #modtime))
  )
  ( primaryKey #id `as` #pk_user_session )
