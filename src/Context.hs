module Context (
  Ctx(..)
  , readContextFromEnv
  , readContextFromEnvWithConnStr
) where

import qualified Data.ByteString.Char8  as BSC
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import qualified Database.Beam.Postgres as Pg
import           System.Environment     (getEnv)
import           Text.Read              (readMaybe)


data Ctx = Ctx {
  conn          :: Pg.Connection,
  port          :: Int,
  apiKey        :: T.Text,
  corsReqOrigin :: T.Text
}

readContextFromEnv :: IO Ctx
readContextFromEnv =
  Ctx <$>
    (BSC.pack <$> getEnv "DBCONN" >>= Pg.connectPostgreSQL) <*>
    (fromMaybe (error "Env var PORT must be set") . readMaybe <$> getEnv "PORT") <*>
    (T.pack <$> getEnv "API_KEY") <*>
    (T.pack <$> getEnv "CORS_ORIGIN")

readContextFromEnvWithConnStr :: T.Text -> IO Ctx
readContextFromEnvWithConnStr conn =
  let connStr = BSC.pack $ T.unpack conn
  in Ctx <$>
      Pg.connectPostgreSQL connStr <*>
      (fromMaybe (error "Env var PORT must be set") . readMaybe <$> getEnv "PORT") <*>
      (T.pack <$> getEnv "API_KEY") <*>
      (T.pack <$> getEnv "CORS_ORIGIN")
