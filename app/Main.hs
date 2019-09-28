module Main where

import           Control.Monad.Extra         (ifM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Except  (ExceptT, throwE)
import           Control.Monad.Trans.Reader  (ReaderT, ask, asks, runReaderT)

import           Data.Aeson                  (ToJSON, FromJSON)
import qualified Data.ByteString.Char8       as BSC

import           Database.Beam               as B
import qualified Database.Beam.Query         as BeamQ
import qualified Database.Beam.Postgres      as Pg
import           Data.Maybe                  (fromMaybe)

import qualified Data.Text      as T
import           GHC.Generics   (Generic)
import Network.Wai.Handler.Warp (setPort, setBeforeMainLoop, defaultSettings, runSettings)

import           Servant
import           Servant.Server     (ServerError(..), err403)

import           System.Environment (getEnv)
import           System.IO          (hPutStrLn, stderr)
import           Text.Read          (readMaybe)


---------------------------------------------------------
import Lib
import Db
import ApiTypes (PageView(..), Event(..), ToDatabase, convertToDb)


type PostAPI =
  Header "Authorization" T.Text :> "event" :> ReqBody '[JSON] Event    :> Post '[JSON] Int :<|>
  Header "Authorization" T.Text :> "page"  :> ReqBody '[JSON] PageView :> Post '[JSON] Int

withAuth :: Maybe T.Text -> AppM a -> AppM a
withAuth auth f =
  ifM (isCorrectAuth auth)
      (f)
      (lift $ Handler $ throwE err403)

server :: ServerT PostAPI AppM
server =
  postEvent :<|>
  postPageView
  where
    postEvent :: Maybe T.Text -> Event -> AppM Int
    postEvent auth event@Event{..} =
      withAuth auth $ do
        Ctx{..} <- ask
        liftIO $ print event
        status <- liftIO $ Pg.runBeamPostgresDebug putStrLn conn $ runInsert $
          insert (dbEvents analyticsDb) $ insertExpressions [convertToDb event]
        liftIO $ print status
        return 1
    postPageView :: Maybe T.Text -> PageView -> AppM Int
    postPageView auth pageview@PageView{..} =
      withAuth auth $ do
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
  conn :: Pg.Connection,
  apiKey :: T.Text
}

type AppM = ReaderT Ctx Handler
--             newtype Handler a = Handler { runHandler' :: ExceptT ServerError IO a }


class (Monad m) => MonadAuth m where
  isCorrectAuth :: Maybe T.Text -> m Bool

instance (Monad m) => MonadAuth (ReaderT Ctx m) where
  isCorrectAuth auth = do
    key <- asks apiKey
    pure $ auth == Just key

main :: IO ()
main = do
  port <- fromMaybe (error "Env var PORT must be set") . readMaybe <$> getEnv "PORT"
  ctx <- Ctx <$>
          (BSC.pack <$> getEnv "DBCONN" >>= Pg.connectPostgreSQL) <*>
          (T.pack <$> getEnv "API_KEY")
  let settings = setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings (app ctx)
