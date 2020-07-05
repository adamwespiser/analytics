module Squeal.Query where

import           ApiTypes
import           Squeal.PostgreSQL
import           Squeal.Schema

insertEventPq :: Statement DB Event ()
insertEventPq = manipulation $ insertInto_ #events $ Values_ $
  Default `as` #id :*
  Set (param @1) `as` #user_session_id  :*
  Set (param @2) `as` #category :*
  Set (param @3) `as` #text :*
  Default `as` #modtime

insertPageViewPq :: Statement DB PageView ()
insertPageViewPq = manipulation $ insertInto_ #page_view $ Values_ $
  Default `as` #id :*
  Set (param @1) `as` #user_session_id :*
  Set (param @2) `as` #url_filepath :*
  Default `as` #modtime

insertSessionPq :: Statement DB () UserSession
insertSessionPq = Manipulation nilParams genericRow $
  insertInto #user_session
  (Values_ (Default `as` #id :* Default `as` #modtime))
  OnConflictDoRaise
  (Returning_ (#id `as` #userSessionId))
