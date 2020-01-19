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
--xjjimport           Servant.QuickCheck
-- import           Servant.QuickCheck.Internal (serverDoesntSatisfy)

import           Test.Hspec
import           Data.Either (isLeft, isRight)
import           Data.Function ((&))
--import           Test.Hspec.Wai
--import           Test.Hspec.Wai.Matcher
import Server (API, app, readContextFromEnv)
import System.IO (stderr, BufferMode(..), hPutStrLn, hSetBuffering, stdout)
import qualified Data.UUID.Types as UUID (nil)

{- API takes Event, PageView, UserSession -}
data Endpoint = EpEvent | EpPageView | EpUserSession

getUrl ::IO BaseUrl
getUrl = parseBaseUrl $ unpack $ "http://localhost:8888/"
  where fsub EpEvent = "event"
        fsub EpPageView = "page"
        fsub EpUserSession = "session"

event :: Event
event = Event {
  evUserSessionId = UUID.nil
  , evCategory = "TEST"
  , evLabel = "LABEL"
  }

pageview :: PageView
pageview = PageView {
  pgUserSessionId = UUID.nil
  , pgUrlFilePath = "/"
  }


gec :: (x :<|> y) -> (x, y)
gec (a :<|> b) = (a, b)

spec :: Spec
spec = withDB $ describe  "withDB works" $ do
      let myapi = client (Proxy :: Proxy API)
      baseUrl <- runIO $ parseBaseUrl $ unpack "127.0.0.1:8888/"
      manager <- runIO $ newManager defaultManagerSettings
      runIO $ C.threadDelay 10000
      let clientEnv = mkClientEnv manager baseUrl
      it "creates db env" $ \(_, config) ->
        Context.port config `shouldBe` 8888
      it "auth fails on empty key" $ \(_, _) -> do
        let eventRoute = fst $ gec myapi
        result <- runClientM (eventRoute  (Just "") event) clientEnv
        result `shouldSatisfy` isLeft
      it "event endpoint works" $ \(_, config) -> do
        let key = Just $ apiKey config
        let eventRoute = fst $ gec myapi
        result <- runClientM (eventRoute key event) clientEnv
        result `shouldBe` Right NoContent
      it "spacer test (XXX)" $ \(_, config) ->
        Context.port config `shouldBe` 8888
      it "pageview endpoint works" $ \(_, config) -> do
        let pageviewRoute = fst $ gec $ snd $ gec myapi
        let key = Just $ apiKey config
        result <- runClientM (pageviewRoute key pageview) clientEnv
        result `shouldBe` Right NoContent
      it "session endpoint works" $ \(_, config) -> do
        let sessionRoute = snd $ gec $ snd $ gec myapi
        let key = Just $ apiKey config
        result <- runClientM (sessionRoute key) clientEnv
        result `shouldSatisfy` (\x -> isRight x
            && case x of {Right (UserSession uuid) -> uuid /= UUID.nil;
                          _                        -> False;} )

