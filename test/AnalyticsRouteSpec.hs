module AnalyticsRouteSpec (AnalyticsRouteSpec.spec) where

import           Helpers (withDB)
import           Context (Ctx(..))
import           Test.Hspec
import Servant
import ApiTypes
import Types

import qualified Control.Concurrent               as C

import qualified Control.Concurrent               as C
import           Control.Concurrent.MVar
import           Control.Exception                (bracket)
import           Data.Aeson
import           Data.Text                        (Text, unpack)
import           GHC.Generics
import           Network.HTTP.Client       hiding (Proxy)
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp         as Warp

import           Servant
import           Servant.Client
import           Servant.Server
-- import           Servant.QuickCheck
-- import           Servant.QuickCheck.Internal (serverDoesntSatisfy)

import           Test.Hspec
--import           Test.Hspec.Wai
--import           Test.Hspec.Wai.Matcher
import Server (API, app, readContextFromEnv)
import qualified Data.UUID.Types as UUID (nil)

withUserApp :: IO () -> IO ()
withUserApp action = do
  ctx <- readContextFromEnv
  bracket (C.forkIO $ Warp.run 8888 $ app ctx)
    C.killThread
    (const action)


{- API takes Event, PageView, UserSession -}
data Endpoint = EpEvent | EpPageView | EpUserSession

getUrl :: Text -> Endpoint -> IO BaseUrl
getUrl key ep = parseBaseUrl $ unpack $ "http://localhost:8888/"
  where fsub EpEvent = "event"
        fsub EpPageView = "page"
        fsub EpUserSession = "session"

event :: Event
event = Event {
  evUserSessionId = UUID.nil
  , evCategory = "TEST"
  , evLabel = "LABEL"
  }

getEventCaller :: ((Maybe Text -> Event -> ClientM NoContent) :<|> (a0 :<|> b0))
 -> (Maybe Text -> Event -> ClientM NoContent)
getEventCaller (f :<|> _ :<|> _) = f

spec :: Spec
spec = withDB $ describe  "withDB works" $ do
    around_ withUserApp $ do
      let myapi = client (Proxy :: Proxy API)
      ctx <- runIO $ readContextFromEnv
      baseUrl <- runIO $ getUrl (apiKey ctx) EpEvent
      manager <- runIO $ newManager defaultManagerSettings
      let clientEnv = mkClientEnv manager baseUrl
      it "creates db env" $ \(_, config) ->
        Context.port config `shouldBe` 8080
{-
      it "auth fails on empty key" $ \(_, config) -> do
        result <- runClientM ((getEventCaller myapi)  (Just "") event) clientEnv
        result `shouldBe` Right NoContent

      it "event endpoint works" $ \(_, config) -> do
        result <- runClientM ((getEventCaller myapi)  (Just $ apiKey ctx) event) clientEnv
        result `shouldBe` Right NoContent
-}

