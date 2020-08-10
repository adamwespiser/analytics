
module Squeal.Schema where

import           Squeal.PostgreSQL ((:::), (:=>), NullType (NotNull),
                                    Optionality (Def, NoDef), PGType (..),
                                    Public, SchemumType (Table),
                                    TableConstraint (PrimaryKey))

type EventsTable =
  '[ "id"              ::: 'Def   :=> 'NotNull 'PGint4
   , "user_session_id" ::: 'NoDef :=> 'NotNull 'PGuuid
   , "category"        ::: 'NoDef :=> 'NotNull 'PGtext
   , "text"            ::: 'NoDef :=> 'NotNull 'PGtext
   , "modtime"         ::: 'Def   :=> 'NotNull 'PGtimestamptz
   ]
type EventConstraints = '[ "pk_events" ::: 'PrimaryKey '["id"]]

type PageViewTable =
  '[ "id"              ::: 'Def   :=> 'NotNull 'PGint4
   , "user_session_id" ::: 'NoDef :=> 'NotNull 'PGuuid
   , "url_filepath"    ::: 'NoDef :=> 'NotNull 'PGtext
   , "modtime"         ::: 'Def   :=> 'NotNull 'PGtimestamptz
  ]
type PageViewConstraints = '["pk_page_view" ::: 'PrimaryKey '["id"]]

type UserSessionTable =
  '[ "id"      ::: 'Def :=> 'NotNull 'PGuuid
   , "modtime" ::: 'Def :=> 'NotNull 'PGtimestamptz
   ]
type UserSessionConstraints = '["pk_user_session" ::: 'PrimaryKey '["id"]]

type Schema =
  '[ "events"       ::: 'Table (EventConstraints :=> EventsTable)
   , "page_view"    ::: 'Table (PageViewConstraints :=> PageViewTable)
   , "user_session" ::: 'Table (UserSessionConstraints :=> UserSessionTable)
  ]

type DB = Public Schema
