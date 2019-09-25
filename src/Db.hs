module Db
    ( PageViewDB
    , EventDB
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


data EventDBT f = EventDB {
  devId                :: Columnar f (SqlSerial Int),
  devSessionTrackingId :: Columnar f T.Text,
  devCategory          :: Columnar f T.Text,
  devLabel             :: Columnar f T.Text,
  devModtime           :: Columnar f LocalTime
} deriving Generic
instance Beamable EventDBT
type EventDB =  EventDBT Identity
type EventId = PrimaryKey EventDBT Identity
instance Table EventDBT where
  data PrimaryKey EventDBT f = EventId (Columnar f (SqlSerial Int))
    deriving (Generic, Beamable)
  primaryKey = EventId . devId


data PageViewDBT f = PageViewDB {
  dpgId                :: Columnar f (SqlSerial Int),
  dpgSessionTrackingID :: Columnar f T.Text,
  dpgUrlFilePath       :: Columnar f T.Text,
  dpgModtime           :: Columnar f LocalTime
} deriving Generic
instance Beamable PageViewDBT
type PageViewDB = PageViewDBT Identity
type PageViewId = PrimaryKey PageViewDBT Identity
instance Table PageViewDBT where
  data PrimaryKey PageViewDBT f = PageViewId (Columnar f (SqlSerial Int))
    deriving (Generic, Beamable)
  primaryKey = PageViewId . dpgId

data AnalyticsDb f  = AnalyticsDb {
  adbEvent :: f (TableEntity EventDBT),
  adbPageView :: f (TableEntity PageViewDBT)
} deriving (Generic, Database be)

analyticsDb :: DatabaseSettings be AnalyticsDb
analyticsDb = defaultDbSettings
