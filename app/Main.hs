module Main where

import           GHC.Generics (Generic)
import           Network.Wai
import           Network.Wai.Handler.Warp
import qualified Data.Text as T
import           Data.Aeson (ToJSON, FromJSON)
-- import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           System.Environment
import           System.IO (hPutStrLn, stderr)
import           Servant
import           Database.Beam as B
import qualified Database.Beam.Query as BeamQ
import qualified Database.Beam.Postgres as Pg
import qualified Data.ByteString.Char8 as BSC
import           Control.Monad.IO.Class
import           Data.Maybe (fromMaybe)
import           Text.Read (readMaybe)
import           Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)

import Lib
import Db


data Event = Event {
  evSessionTrackingId :: T.Text,
  evCategory :: T.Text,
  evLabel :: T.Text
} deriving (Eq, Show, Generic)
instance ToJSON Event
instance FromJSON Event

data PageView = PageView {
  pgSessionTrackingID :: T.Text,
  pgUrlFilePath :: T.Text
} deriving (Eq, Show, Generic)
instance ToJSON PageView
instance FromJSON PageView

class ToDatabase a b where
  convertToDb :: a -> b

instance ToDatabase PageView (PageViewDBT (QExpr Pg.Postgres s)) where
  convertToDb pageview@PageView{..} =
    PageViewDB
      B.default_
      (BeamQ.val_ pgSessionTrackingID)
      (BeamQ.val_ pgUrlFilePath)
      Pg.now_

instance ToDatabase Event (EventsDBT (QExpr Pg.Postgres s)) where
  convertToDb event@Event{..} =
    EventsDB
      B.default_
      (BeamQ.val_ evSessionTrackingId)
      (BeamQ.val_ evCategory)
      (BeamQ.val_ evLabel)
      Pg.now_


type PostAPI =
  "event" :> ReqBody '[JSON] Event    :> Post '[JSON] Int :<|>
  "page"  :> ReqBody '[JSON] PageView :> Post '[JSON] Int

server :: ServerT PostAPI AppM
server =
  postEvent :<|>
  postPageView
  where
    postEvent :: Event -> AppM Int
    postEvent event@Event{..} = do
      Ctx{..} <- ask
      liftIO $ print event
      status <- liftIO $ Pg.runBeamPostgresDebug putStrLn conn $ runInsert $
        insert (dbEvents analyticsDb) $ insertExpressions [convertToDb event]
      liftIO $ print status
      return 1
    postPageView :: PageView -> AppM Int
    postPageView pageview@PageView{..} = do
      Ctx{..} <- ask
      liftIO $ print pageview
      status  <- liftIO $ Pg.runBeamPostgresDebug putStrLn conn $ runInsert $
        insert (dbPageView analyticsDb) $ insertExpressions [convertToDb pageview]
      return 1


api :: Proxy PostAPI
api = Proxy

app :: Ctx -> Application
app s = serve api $ hoistServer api (`runReaderT` s) server

data Ctx = Ctx {
  port :: Int,
  conn :: Pg.Connection
}

type AppM = ReaderT Ctx Handler

main :: IO ()
main = do
  port <- fromMaybe (error "Env var PORT must be set") . readMaybe <$> getEnv "PORT"
  connectionStr <- BSC.pack <$> getEnv "DBCONN"
  conn <- Pg.connectPostgreSQL connectionStr
  let settings = setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  let ctx = Ctx port conn
  runSettings settings (app ctx)
