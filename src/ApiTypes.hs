module ApiTypes (
    Event(..)
  , PageView(..)
  , UserSession(..)
) where

import           Data.Aeson      (FromJSON, ToJSON)
import qualified Data.Text       as T
import           Data.UUID.Types (UUID)
import qualified Generics.SOP    as SOP
import qualified GHC.Generics    as GHC

newtype UserSession = UserSession {
  userSessionId :: UUID
} deriving stock (Eq, Show, GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
instance ToJSON UserSession
instance FromJSON UserSession

data Event = Event {
  evUserSessionId :: UUID,
  evCategory      :: T.Text,
  evLabel         :: T.Text
} deriving (Eq, Show, GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
instance ToJSON Event
instance FromJSON Event

data PageView = PageView {
  pgUserSessionId :: UUID,
  pgUrlFilePath   :: T.Text
} deriving (Eq, Show, GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
instance ToJSON PageView
instance FromJSON PageView

