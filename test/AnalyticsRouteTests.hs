module AnalyticsRouteTests (spec) where

import           Helpers (withDB)
import           Context (Ctx(..))
import Test.Hspec


spec :: Spec
spec = withDB $ describe  "withDB works" $
    it "creates db env" $ \(_, config) ->
      (port config) `shouldBe` 8080


