module Context (
  Ctx(..)
  , CtxTest(..)
  , readContextFromEnv
  , readContextFromEnvWithConnStr
) where

import qualified Data.ByteString.Char8 as BSC
import           Data.Maybe            (fromMaybe)
import qualified Data.Text             as T
import           Squeal.PostgreSQL     (Connection, K, Pool,
                                        createConnectionPool)
import           Squeal.Schema         (DB)
import           System.Environment    (getEnv)
import           Text.Read             (readMaybe)
import qualified Database.PostgreSQL.Simple as PG

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

data CtxTest = CtxTest {
  connT          :: PG.Connection,
  portT          :: Int,
  apiKeyT        :: T.Text,
  connStrT       :: BSC.ByteString --for migration
}

readContextFromEnvWithConnStr :: T.Text -> IO CtxTest
readContextFromEnvWithConnStr conn =
  let connStr = BSC.pack $ T.unpack conn
  in CtxTest <$>
      PG.connectPostgreSQL connStr <*>
      (fromMaybe (error "Env var PORT must be set") . readMaybe <$> getEnv "PORT") <*>
      (T.pack <$> getEnv "API_KEY") <*>
      (BSC.pack <$> getEnv "DBCONN")
