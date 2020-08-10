module AnalyticsRouteSpec (AnalyticsRouteSpec.spec) where

import           ApiTypes
import           Context             (CtxTest (..))
import           Helpers             (withDB)

import           Data.Text           (unpack)
import           Network.HTTP.Client hiding (Proxy)

import           Servant
import           Servant.Client

import qualified Data.UUID.Types     as UUID (nil)
import           Server              (API)
import           Test.Hspec
import           Utils               (isLeft, isRight)

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
spec =
  withDB $ do
    let myapi = client (Proxy :: Proxy API)
    baseUrl <- runIO $ parseBaseUrl $ unpack "127.0.0.1:8888/"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager baseUrl
    describe "withDB works" $ do
      it "creates db env" $ \(_, config) ->
        Context.portT config `shouldBe` 8888
    describe "auth" $ do
      it "auth fails on empty key" $ \(_, _) -> do
        let eventRoute = fst $ gec myapi
        result <- runClientM (eventRoute  (Just "") event) clientEnv
        result `shouldSatisfy` isLeft
    describe "event" $ do
      it "event endpoint works" $ \(_, config) -> do
        let key = Just $ apiKeyT config
        let eventRoute = fst $ gec myapi
        result <- runClientM (eventRoute key event) clientEnv
        result `shouldBe` Right NoContent
      it "spacer test (XXX)" $ \(_, config) ->
        Context.portT config `shouldBe` 8888
    describe "pageview" $ do
      it "pageview endpoint works" $ \(_, config) -> do
        let pageviewRoute = fst $ gec $ snd $ gec myapi
        let key = Just $ apiKeyT config
        result <- runClientM (pageviewRoute key pageview) clientEnv
        result `shouldBe` Right NoContent
    describe "session" $ do
      it "session endpoint works" $ \(_, config) -> do
        let sessionRoute = snd $ gec $ snd $ gec myapi
        let key = Just $ apiKeyT config
        result <- runClientM (sessionRoute key) clientEnv
        result `shouldSatisfy` (\x -> isRight x
            && case x of {Right (UserSession uuid) -> uuid /= UUID.nil;
                          _                        -> False;} )

