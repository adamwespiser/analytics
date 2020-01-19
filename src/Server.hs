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
    headMay :: [a] -> Maybe a
    headMay = Prelude.foldr (\x _ -> Just x) Nothing

app :: Ctx -> Application
app ctx = logStdoutDev $
  cors (const $ Just policy) $
  provideOptions apiProxy $
  serve apiProxy $ hoistServer apiProxy (`runReaderT` ctx) server
    where
      apiProxy = Proxy @API
      policy = simpleCorsResourcePolicy
                { corsRequestHeaders = [ "content-type" ] }

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

