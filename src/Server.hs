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
import           Servant.API.Generic
import           Servant.Server.Generic

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

data Routes route = Routes
 { event :: route :- "event" :> QueryParam "auth" T.Text :> ReqBody '[JSON] Event :> Post '[JSON] NoContent
 , page :: route :- "page" :> QueryParam "auth" T.Text :> ReqBody '[JSON] PageView :> Post '[JSON] NoContent
 , session :: route :- "session" :> QueryParam "auth" T.Text :> Get '[JSON] UserSession
 } deriving (Generic)

server :: Routes (AsServerT (AppM Ctx))
server = Routes
 { event = postEvent
 , page = postPageView
 , session = getUserSession
 }
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
    headMay :: [a] -> Maybe a
    headMay = Prelude.foldr (\x _ -> Just x) Nothing

{-
server' :: ServerT Routes (AppM Ctx)
server' =
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
    headMay :: [a] -> Maybe a
    headMay = Prelude.foldr (\x _ -> Just x) Nothing
-}

app :: Ctx -> Application
app ctx = logStdoutDev $
  cors (const $ Just policy) $
  provideOptions apiProxy $
  genericServeT (natTrans ctx) server
  where
      apiProxy :: Proxy (ToServantApi Routes)
      apiProxy = genericApi (Proxy :: Proxy Routes)
      policy = simpleCorsResourcePolicy
                { corsRequestHeaders = [ "content-type" ] }

natTrans :: ctx -> AppM ctx a -> Handler a
natTrans ctx x = runReaderT x ctx


--type ApiProxy =(ToServantApi Routes)

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

