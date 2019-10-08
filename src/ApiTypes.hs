module ApiTypes (
    Event(..)
  , PageView(..)
  , UserSession(..)
  , ToDatabase
  , convertToDb
) where

import           Data.Aeson (ToJSON, FromJSON)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Database.Beam as B
import qualified Database.Beam.Query as BeamQ
import qualified Database.Beam.Postgres as Pg
------------------------------------------------------
import           Db (
    EventsDBT(..)
  , PageViewDBT(..)
  , UserSessionDBT(..)
  )


class ToDatabase a b where
  convertToDb :: a -> b


data UserSession = UserSession {
  usSessionTrackingId :: !Int
} deriving (Eq, Show, Generic)
instance ToJSON UserSession
instance FromJSON UserSession

data Event = Event {
  evSessionTrackingId :: Int,
  evCategory :: T.Text,
  evLabel :: T.Text
} deriving (Eq, Show, Generic)
instance ToJSON Event
instance FromJSON Event

data PageView = PageView {
  pgSessionTrackingId :: Int,
  pgUrlFilePath :: T.Text
} deriving (Eq, Show, Generic)
instance ToJSON PageView
instance FromJSON PageView


instance ToDatabase UserSession (UserSessionDBT (BeamQ.QExpr Pg.Postgres s)) where
  convertToDb _ = UserSessionDB B.default_ Pg.now_


instance ToDatabase PageView (PageViewDBT (BeamQ.QExpr Pg.Postgres s)) where
  convertToDb pageview@PageView{..} =
    PageViewDB
      B.default_
      (BeamQ.val_ pgSessionTrackingId)
      (BeamQ.val_ pgUrlFilePath)
      Pg.now_

instance ToDatabase Event (EventsDBT (BeamQ.QExpr Pg.Postgres s)) where
  convertToDb event@Event{..} =
    EventsDB
      B.default_
      (BeamQ.val_ evSessionTrackingId)
      (BeamQ.val_ evCategory)
      (BeamQ.val_ evLabel)
      Pg.now_

