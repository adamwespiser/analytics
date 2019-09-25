module Main where

import GHC.Generics (Generic)
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.Text as T
import Data.Aeson (ToJSON, FromJSON)
-- import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Environment
import System.IO (hPutStrLn, stderr)
import Servant
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Lib
import Db
import           Control.Monad.IO.Class


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



type PostAPI =
  "event" :> ReqBody '[JSON] Event    :> Post '[JSON] Int :<|>
  "page"  :> ReqBody '[JSON] PageView :> Post '[JSON] Int

server :: Server PostAPI
server =
  postEvent :<|>
  postPageView
  where
    postEvent :: Event -> Handler Int
    postEvent event = do
      liftIO $ print event
      return 1
    postPageView :: PageView -> Handler Int
    postPageView pageview = do
      liftIO $ print pageview
      return 1

postApi :: Proxy PostAPI
postApi = Proxy

mkApp :: IO Application
mkApp = return $ serve postApi server


main :: IO ()
main = do
  port <- fromMaybe (error "Env var PORT must be set") . readMaybe <$> getEnv "PORT"

  let settings = setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp
