module Server (
  API
  , readContextFromEnv
  , runAppWithContext
  , app
  , server
  , runMain
) where

import           Control.Monad.IO.Class                 (liftIO)
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
import           Db
import           Types                                  (AppM, getContext,
                                                         insertEvent,
                                                         insertPageView,
                                                         insertUserSession,
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

server :: Routes (AsServerT (AppM Ctx))
server = Routes
 { event
 , page
 , session
 }
  where
    event :: Maybe T.Text -> Event -> AppM Ctx NoContent
    event auth evt@Event{..} =
      withAuth auth $ do
        Ctx{ conn } <- getContext
        liftIO $ print evt
        insertEvent conn evt
        return NoContent
    page :: Maybe T.Text -> PageView -> AppM Ctx NoContent
    page auth pageview@PageView{..} =
      withAuth auth $ do
        Ctx{ conn } <- getContext
        liftIO $ print pageview
        insertPageView conn pageview
        return NoContent
    session :: Maybe T.Text -> AppM Ctx UserSession
    session auth =
      withAuth auth $ do
        Ctx{ conn } <- getContext
        status <- insertUserSession conn
        return $ UserSession $ (usersessionId . getSingleResult) status
    getSingleResult lst =
        -- TODO code smell: headMay then toss an error?
        fromMaybe (error $ "storeRun: single item not returned: " ++ show lst )
            $ Utils.headMay lst

app :: Ctx -> Application
app ctx = logStdoutDev $
  cors (const $ Just policy) $
  provideOptions apiProxy $
  genericServeT (natTrans ctx) server
  where
      apiProxy :: Proxy API
      apiProxy = genericApi (Proxy :: Proxy Routes)
      policy = simpleCorsResourcePolicy
                { corsRequestHeaders = [ "content-type" ] }

type API = ToServantApi Routes

natTrans :: ctx -> AppM ctx a -> Handler a
natTrans ctx x = runReaderT x ctx

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

