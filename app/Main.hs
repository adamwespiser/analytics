module Main where

import           Control.Monad.Extra         (ifM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Except  (throwE)
import           Control.Monad.Trans.Reader  (ReaderT, ask, asks, runReaderT)

import qualified Data.ByteString.Char8       as BSC

import           Database.Beam               as B
import qualified Database.Beam.Postgres      as Pg
import           Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text      as T
import           Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))
-- import Network.Wai.Middleware.Servant.Options



import Network.Wai.Handler.Warp (setPort, setBeforeMainLoop, defaultSettings, runSettings)
import           Safe                   (headMay)

import           Servant
import           Servant.Server     (err403)

import           System.Environment (getEnv)
import           System.IO          (hPutStrLn, stderr)
import           Text.Read          (readMaybe)
import qualified Network.Wai as WAI


---------------------------------------------------------
import Db
import ApiTypes (
  PageView(..)
  , Event(..)
  , UserSession(..)
  , convertToDb
  )

type API
  = "event"
      :> QueryParam "auth" T.Text
      :> ReqBody '[JSON] Event
      :> Post '[JSON] NoContent
    :<|> "page"
      :> QueryParam "auth" T.Text
      :> ReqBody '[JSON] PageView
      :> Post '[JSON] NoContent
    :<|> "session"
      :> QueryParam "auth" T.Text
      :> Get '[JSON] UserSession

withAuth :: Maybe T.Text -> AppM a -> AppM a
withAuth auth f =
  ifM (isCorrectAuth auth) f
      (lift $ Handler $ throwE err403)


server :: ServerT API AppM
server =
  postEvent :<|>
  postPageView :<|>
  getUserSession
  where
    postEvent :: Maybe T.Text -> Event -> AppM NoContent
    postEvent auth event@Event{..} =
      withAuth auth $ do
        Ctx{..} <- ask
        liftIO $ print event
        status <- liftIO $ Pg.runBeamPostgresDebug putStrLn conn $ runInsert $
          insert (dbEvents analyticsDb) $ insertExpressions [convertToDb event]
        liftIO $ print status
        return NoContent
    postPageView :: Maybe T.Text -> PageView -> AppM NoContent
    postPageView auth pageview@PageView{..} =
      withAuth auth $ do
        Ctx{..} <- ask
        liftIO $ print pageview
        status  <- liftIO $ Pg.runBeamPostgresDebug putStrLn conn $ runInsert $
          insert (dbPageView analyticsDb) $ insertExpressions [convertToDb pageview]
        liftIO $ print status
        return NoContent
    getUserSession :: Maybe T.Text -> AppM UserSession
    getUserSession auth =
      withAuth auth $ do
        Ctx{..} <- ask
        status  <- liftIO $ Pg.runBeamPostgresDebug putStrLn conn  $ do
          insertValue <- runInsertReturningList $ insert (dbUserSession analyticsDb) $ insertExpressions [UserSessionDB B.default_ Pg.now_]
          liftIO $ print insertValue
          pure insertValue
        return $ UserSession $  ( usersessionId . getSingleResult) status
    getSingleResult lst =
        fromMaybe (error $ "storeRun: single item not returned: " ++ show lst )
            $ headMay lst

app :: Ctx -> Application
app s = serve (Proxy @API) $ hoistServer (Proxy @API) (`runReaderT` s) server

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
  cors_origin <- getEnv "CORS_ORIGIN"

  ctx <- Ctx <$>
          (BSC.pack <$> getEnv "DBCONN" >>= Pg.connectPostgreSQL) <*>
          (T.pack <$> getEnv "API_KEY")
  let myCors :: WAI.Middleware
      myCors = cors (const $ Just $ simpleCorsResourcePolicy
        {corsOrigins    = cors_origin_fn cors_origin,
         corsVaryOrigin = True,
         corsMethods = ["GET", "HEAD", "POST", "DELETE", "PUT"] } )

  let settings = setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings $ myCors (app ctx)
  where
  -- take the inputed cors origin and format it,
  -- defaulting to Nothing, which is all origins allowed, if there is
  -- if CORS_ORIGIN is "" or "*"
  cors_origin_fn :: String -> Maybe ([BSC.ByteString], Bool)
  cors_origin_fn = \case
      ""  -> Nothing
      "*" -> Nothing
      x   -> Just ([BSC.pack x], True)
