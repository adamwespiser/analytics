module Main where

import           Control.Monad.Extra         (ifM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Except  (throwE)
import           Control.Monad.Trans.Reader  (ReaderT, ask, asks, runReaderT)

import           Database.Beam               as B
import qualified Database.Beam.Postgres      as Pg
import           Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text      as T
import           Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))
import           Network.Wai.Middleware.Servant.Options
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)


import Network.Wai.Handler.Warp (setPort, setBeforeMainLoop, defaultSettings, runSettings)
import           Safe                   (headMay)

import           Servant
import           Servant.Server     (err403)

import           System.IO          (hPutStrLn, stderr)


---------------------------------------------------------
import Db
import Context (Ctx(..), readContextFromEnv)
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
app ctx = logStdoutDev $
  cors (const $ Just policy) $
  provideOptions apiProxy $
  serve apiProxy $ hoistServer apiProxy (`runReaderT` ctx) server
      where
      apiProxy = Proxy @API
      policy = simpleCorsResourcePolicy
                { corsRequestHeaders = [ "content-type" ] }




type AppM = ReaderT Ctx Handler


class (Monad m) => MonadAuth m where
  isCorrectAuth :: Maybe T.Text -> m Bool

instance (Monad m) => MonadAuth (ReaderT Ctx m) where
  isCorrectAuth auth = do
    key <- asks apiKey
    pure $ auth == Just key

main :: IO ()
main = do
  ctx <- readContextFromEnv

  let settings = setPort (port ctx) $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show (port ctx))) $
        defaultSettings
  runSettings settings  (app ctx)

