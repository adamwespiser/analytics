module Context (
  Ctx(..)
  , readContextFromEnv
) where

import qualified Database.Beam.Postgres      as Pg
import qualified Data.ByteString.Char8       as BSC
import qualified Data.Text as T
import           System.Environment  (getEnv)
import           Data.Maybe          (fromMaybe)
import           Text.Read           (readMaybe)


data Ctx = Ctx {
  conn :: Pg.Connection,
  port :: Int,
  apiKey :: T.Text,
  corsReqOrigin  :: T.Text
}

readContextFromEnv :: IO Ctx
readContextFromEnv =
  Ctx <$>
    (BSC.pack <$> getEnv "DBCONN" >>= Pg.connectPostgreSQL) <*>
    (fromMaybe (error "Env var PORT must be set") . readMaybe <$> getEnv "PORT") <*>
    (T.pack <$> getEnv "API_KEY") <*>
    (T.pack <$> getEnv "CORS_ORIGIN")
