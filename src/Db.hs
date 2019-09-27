module Db
    (
      PageViewDBT(..)
    , EventsDBT(..)
    , AnalyticsDb(..)
    , analyticsDb
    ) where

import GHC.Generics (Generic)
import Database.Beam.Schema (
    Beamable
  , Columnar
  , defaultDbSettings
  , Database
  , DatabaseSettings
  , PrimaryKey
  , primaryKey
  , Table
  , TableEntity
  )
import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import Data.Time.LocalTime (LocalTime)
import Database.Beam.Backend.SQL.BeamExtensions (SqlSerial)
import qualified Database.Beam.Postgres as Pg
import Database.Beam.Schema
import Database.Beam as B


data EventsDBT f = EventsDB {
  eventsId                :: C f (SqlSerial Int),
  eventsSessionTrackingId :: C f T.Text,
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
  pageviewSessionTrackingId :: C f T.Text,
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
  dbPageView :: f (TableEntity PageViewDBT)
} deriving (Generic, Database be)

analyticsDb :: DatabaseSettings be AnalyticsDb
analyticsDb =
  defaultDbSettings `withDbModification`
    dbModification {
      dbEvents = setEntityName "events"
      , dbPageView = setEntityName "page_view"
    }

