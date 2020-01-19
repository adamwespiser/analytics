module ApiTypes (
    Event(..)
  , PageView(..)
  , UserSession(..)
  , ToDatabase
  , convertToDb
) where

import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Text              as T
import           Data.UUID.Types        (UUID)
import           Database.Beam          as B
import qualified Database.Beam.Postgres as Pg
import qualified Database.Beam.Query    as BeamQ
import           GHC.Generics           (Generic)

------------------------------------------------------
import           Db                     (EventsDBT (..), PageViewDBT (..),
                                         UserSessionDBT (..))

class ToDatabase a b where
  convertToDb :: a -> b

newtype UserSession = UserSession {
  userSessionId :: UUID
} deriving (Eq, Show, Generic)
instance ToJSON UserSession
instance FromJSON UserSession

data Event = Event {
  evUserSessionId :: UUID,
  evCategory      :: T.Text,
  evLabel         :: T.Text
} deriving (Eq, Show, Generic)
instance ToJSON Event
instance FromJSON Event

data PageView = PageView {
  pgUserSessionId :: UUID,
  pgUrlFilePath   :: T.Text
} deriving (Eq, Show, Generic)
instance ToJSON PageView
instance FromJSON PageView


instance ToDatabase UserSession (UserSessionDBT (BeamQ.QExpr Pg.Postgres s)) where
  convertToDb _ = UserSessionDB B.default_ Pg.now_


instance ToDatabase PageView (PageViewDBT (BeamQ.QExpr Pg.Postgres s)) where
  convertToDb PageView{..} =
    PageViewDB
      B.default_
      (BeamQ.val_ pgUserSessionId)
      (BeamQ.val_ pgUrlFilePath)
      Pg.now_

instance ToDatabase Event (EventsDBT (BeamQ.QExpr Pg.Postgres s)) where
  convertToDb Event{..} =
    EventsDB
      B.default_
      (BeamQ.val_ evUserSessionId)
      (BeamQ.val_ evCategory)
      (BeamQ.val_ evLabel)
      Pg.now_

