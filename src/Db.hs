module Db
    (
      PageViewDBT(..)
    , PageViewId
    , EventsDBT(..)
    , EventsId
    , UserSessionDBT(..)
    , UserSessionId
    , AnalyticsDb(..)
    , analyticsDb
    ) where

import GHC.Generics (Generic)
import Database.Beam.Schema (
    Beamable
  , Columnar
  , C
  , dbModification
  , defaultDbSettings
  , Database
  , DatabaseSettings
  , PrimaryKey
  , primaryKey
  , Table
  , TableEntity
  , withDbModification
  , setEntityName
  )
import           Data.Functor.Identity (Identity)
import qualified Data.Text as T
import           Data.Time.LocalTime (LocalTime)
import           Database.Beam.Backend.SQL.BeamExtensions (SqlSerial)
import           Data.UUID.Types (UUID)


data UserSessionDBT f = UserSessionDB {
  usersessionId      :: C f UUID,
  usersessionModtime :: C f LocalTime
} deriving (Generic)
instance Beamable UserSessionDBT
type UserSessionDB = UserSessionDBT Identity
type UserSessionId = PrimaryKey UserSessionDBT Identity
instance Table UserSessionDBT where
  data PrimaryKey UserSessionDBT f = UserSessionId (Columnar f UUID)
    deriving (Generic, Beamable)
  primaryKey = UserSessionId . usersessionId
deriving instance Show UserSessionDB

data EventsDBT f = EventsDB {
  eventsId                :: C f (SqlSerial Int),
  eventsUserSessionId     :: C f UUID,
  eventsCategory          :: C f T.Text,
  eventsLabel             :: C f T.Text,
  eventsModtime           :: C f LocalTime
} deriving Generic
instance Beamable EventsDBT
type EventsDB =  EventsDBT Identity
type EventsId = PrimaryKey EventsDBT Identity
instance Table EventsDBT where
  data PrimaryKey EventsDBT f = EventsId (Columnar f (SqlSerial Int))
    deriving (Generic, Beamable)
  primaryKey = EventsId . eventsId
deriving instance Show EventsDB


data PageViewDBT f = PageViewDB {
  pageviewId                :: C f (SqlSerial Int),
  pageviewUserSessionId     :: C f UUID,
  pageviewUrlFilepath       :: C f T.Text,
  pageviewModtime           :: C f LocalTime
} deriving (Generic, Beamable)
type PageViewDB = PageViewDBT Identity
type PageViewId = PrimaryKey PageViewDBT Identity
instance Table PageViewDBT where
  data PrimaryKey PageViewDBT f = PageViewId (Columnar f (SqlSerial Int))
    deriving (Generic, Beamable)
  primaryKey = PageViewId . pageviewId
deriving instance Show PageViewDB

data AnalyticsDb f  = AnalyticsDb {
  dbEvents :: f (TableEntity EventsDBT),
  dbPageView :: f (TableEntity PageViewDBT),
  dbUserSession :: f (TableEntity UserSessionDBT)
} deriving (Generic, Database be)

analyticsDb :: DatabaseSettings be AnalyticsDb
analyticsDb =
  defaultDbSettings `withDbModification`
    dbModification {
        dbEvents      = setEntityName "events"
      , dbPageView    = setEntityName "page_view"
      , dbUserSession = setEntityName "user_session"
    }

