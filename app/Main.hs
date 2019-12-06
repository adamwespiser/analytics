module Main where

import           Control.Monad.Extra         (ifM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Except  (throwE)
import           Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)

import           Database.Beam               as B
import qualified Database.Beam.Postgres      as Pg
import           Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text      as T
import qualified Data.UUID.Types as UUID (nil)
import           Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))
import           Network.Wai.Middleware.Servant.Options
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Network.Wai.Handler.Warp (setPort, setBeforeMainLoop, defaultSettings, runSettings)
import           Safe                   (headMay)

import           Servant
import           Servant.Server     (err403)

import           System.IO          (hPutStrLn, stderr, hSetBuffering, stdout, NoBuffering)


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


server :: ServerT API (AppM Ctx)
server =
  postEvent :<|>
  postPageView :<|>
  getUserSession
  where
    postEvent :: Maybe T.Text -> Event -> AppM Ctx NoContent
    postEvent auth event@Event{..} =
      withAuth auth $ do
        Ctx{..} <- getContext
        liftIO $ print event
        insertEvent conn event
        return NoContent
    postPageView :: Maybe T.Text -> PageView -> AppM Ctx NoContent
    postPageView auth pageview@PageView{..} =
      withAuth auth $ do
        Ctx{..} <- getContext
        liftIO $ print pageview
        insertPageView conn pageview
        return NoContent
    getUserSession :: Maybe T.Text -> AppM Ctx UserSession
    getUserSession auth =
      withAuth auth $ do
        Ctx{..} <- getContext
        status <- insertUserSession conn
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


type AppM ctx = ReaderT ctx Handler

class (Monad m) => MonadAuth m where
  withAuth :: Maybe T.Text -> m a -> m a

instance MonadAuth (AppM Ctx) where
  withAuth auth f =
    ifM (isCorrectAuth auth) f
        (lift $ Handler $ throwE err403)
    where
      isCorrectAuth :: Maybe T.Text -> AppM Ctx Bool
      isCorrectAuth auth' = do
        Ctx{..} <- getContext
        pure $ auth' == Just apiKey

class Monad m => HasContext m where
  getContext :: m Ctx
instance HasContext (AppM Ctx) where
  getContext = ask

class Monad m => MonadDb m where
  insertUserSession :: Pg.Connection -> m [UserSessionDBT Identity]
  fetchUserSession :: Pg.Connection -> m UserSession
  insertPageView :: Pg.Connection -> PageView -> m ()
  insertEvent :: Pg.Connection -> Event -> m ()

instance MonadDb (AppM Ctx)  where
  insertUserSession conn = liftIO $ Pg.runBeamPostgresDebug putStrLn conn  $ do
    insertValue <- runInsertReturningList $ insert (dbUserSession analyticsDb) $ insertExpressions [UserSessionDB B.default_ Pg.now_]
    liftIO $ print insertValue
    pure insertValue
  fetchUserSession _ = liftIO $ return $ UserSession UUID.nil
  insertPageView conn pageview = liftIO $ Pg.runBeamPostgresDebug putStrLn conn $ runInsert $
    insert (dbPageView analyticsDb) $ insertExpressions [convertToDb pageview]
  insertEvent conn event = liftIO $ Pg.runBeamPostgresDebug putStrLn conn $ runInsert $
    insert (dbEvents analyticsDb) $ insertExpressions [convertToDb event]


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  ctx <- readContextFromEnv

  let settings = setPort (port ctx) $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show (port ctx))) $
        defaultSettings
  runSettings settings (app ctx)

runAppWithContext :: Ctx -> IO ()
runAppWithContext ctx =
  let settings = setPort (port ctx) $ defaultSettings
  in runSettings settings (app ctx)
