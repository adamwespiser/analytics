module Context (
  Ctx(..)
  , readContextFromEnv
  , readContextFromEnvWithConnStr
) where

import qualified Data.ByteString.Char8 as BSC
import           Data.Maybe            (fromMaybe)
import qualified Data.Text             as T
import           Squeal.PostgreSQL
import           Squeal.Schema         (DB)
import           System.Environment    (getEnv)
import           Text.Read             (readMaybe)

data Ctx = Ctx {
  conn          :: Pool (K Connection DB),
  port          :: Int,
  apiKey        :: T.Text,
  corsReqOrigin :: T.Text,
  connStr       :: BSC.ByteString --for migration
}

defaultMakePool :: BSC.ByteString ->  IO (Pool (K Connection DB))
defaultMakePool connStr = createConnectionPool connStr 5 10 10

readContextFromEnv :: IO Ctx
readContextFromEnv =
  Ctx <$>
    (BSC.pack <$> getEnv "DBCONN" >>= defaultMakePool) <*>
    (fromMaybe (error "Env var PORT must be set") . readMaybe <$> getEnv "PORT") <*>
    (T.pack <$> getEnv "API_KEY") <*>
    (T.pack <$> getEnv "CORS_ORIGIN") <*>
    (BSC.pack <$> getEnv "DBCONN")

readContextFromEnvWithConnStr :: T.Text -> IO Ctx
readContextFromEnvWithConnStr conn =
  let connStr = BSC.pack $ T.unpack conn
  in Ctx <$>
      (defaultMakePool connStr) <*>
      (fromMaybe (error "Env var PORT must be set") . readMaybe <$> getEnv "PORT") <*>
      (T.pack <$> getEnv "API_KEY") <*>
      (T.pack <$> getEnv "CORS_ORIGIN") <*>
      (BSC.pack <$> getEnv "DBCONN")
