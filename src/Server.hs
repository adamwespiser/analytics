module Server (
  API
  , readContextFromEnv
  , runAppWithContext
  , app
  , server
  , runMain
) where

import           Control.Monad.IO.Class                 (MonadIO, liftIO)
import           Control.Monad.Trans.Reader             (runReaderT)
import           Data.Maybe                             (fromMaybe)
import qualified Data.Text                              as T
import           Network.Wai.Middleware.Cors            (CorsResourcePolicy (..),
                                                         cors,
                                                         simpleCorsResourcePolicy)
import           Network.Wai.Middleware.RequestLogger   (logStdoutDev)
import           Network.Wai.Middleware.Servant.Options

import           Network.Wai.Handler.Warp               (defaultSettings,
                                                         runSettings,
                                                         setBeforeMainLoop,
                                                         setPort)
import           Servant
import           Servant.API.Generic                    ((:-), Generic,
                                                         ToServantApi,
                                                         genericApi)
import           Servant.Server.Generic                 (AsServerT,
                                                         genericServeT)

import           System.IO                              (BufferMode (..),
                                                         hPutStrLn,
                                                         hSetBuffering, stderr,
                                                         stdout)

---------------------------------------------------------
import           ApiTypes                               (Event (..),
                                                         PageView (..),
                                                         UserSession (..))
import           Context                                (Ctx (..),
                                                         readContextFromEnv)
import           Control.Monad                          ((<=<))
import qualified Data.UUID.Types                        as UUID (nil)
import qualified Squeal.PostgreSQL                      as Sq
import           Squeal.Query                           (insertEventPq,
                                                         insertPageViewPq,
                                                         insertSessionPq)
import           Squeal.Schema                          (DB)
import           Types                                  (App, HasContext,
                                                         MonadAuth, getContext,
                                                         runAppInTransaction,
                                                         withAuth)
import qualified Utils                                  (headMay)

data Routes route = Routes
 { event :: route
     :- "event"
     :> QueryParam "auth" T.Text
     :> ReqBody '[JSON] Event
     :> Post '[JSON] NoContent
 , page  :: route
     :- "page"
     :> QueryParam "auth" T.Text
     :> ReqBody '[JSON] PageView
     :> Post '[JSON] NoContent
 , session :: route
     :- "session"
     :> QueryParam "auth" T.Text
     :> Get '[JSON] UserSession
 } deriving (Generic)

server :: Routes (AsServerT App)
server = Routes
 { event
 , page
 , session
 }
  where
    event ::
         ( Monad m
         , MonadIO m
         , HasContext m
         , MonadAuth m
         , Sq.MonadPQ DB m)
       => Maybe T.Text -> Event -> m NoContent
    event auth evt@Event{..} =
      withAuth auth $ do
        Ctx{ conn } <- getContext
        Sq.executeParams insertEventPq evt
        return NoContent
    page ::
        ( Monad m
        , MonadIO m
        , HasContext m
        , MonadAuth m
        , Sq.MonadPQ DB m)
      => Maybe T.Text -> PageView -> m NoContent
    page auth pageview@PageView{..} =
      withAuth auth $ do
        Ctx{ conn } <- getContext
        Sq.executeParams insertPageViewPq pageview
        return NoContent
    session ::
        ( Monad m
        , MonadIO m
        , HasContext m
        , MonadAuth m
        , Sq.MonadPQ DB m)
      => Maybe T.Text -> m UserSession
    session auth =
      withAuth auth $ do
        Ctx{ conn } <- getContext
        Sq.execute insertSessionPq
        return $ UserSession UUID.nil

app :: Ctx -> Application
app ctx = logStdoutDev $
  cors (const $ Just policy) $
  provideOptions apiProxy $
  genericServeT toHandler server
  where
    apiProxy :: Proxy API
    apiProxy = genericApi (Proxy :: Proxy Routes)
    policy = simpleCorsResourcePolicy
              { corsRequestHeaders = [ "content-type" ] }
    toHandler :: App a -> Handler a
    toHandler =
      either throwError pure
      <=< liftIO
      .   fmap Right
      .   runAppInTransaction ctx

type API = ToServantApi Routes

runAppWithContext :: Ctx -> IO ()
runAppWithContext ctx =
  let settings = setPort (port ctx) $ defaultSettings
  in runSettings settings (app ctx)

runMain :: IO ()
runMain = do
  hSetBuffering stdout NoBuffering
  ctx <- readContextFromEnv
  let settings = setPort (port ctx) $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show (port ctx))) $
        defaultSettings
  runSettings settings (app ctx)
