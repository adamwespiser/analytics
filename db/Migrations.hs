import Database.PostgreSQL.Simple (withTransaction)
import Database.PostgreSQL.Simple.Migration (
  runMigration
  , MigrationContext(..)
  , MigrationCommand(..)
  , MigrationResult(..)
  )

-- import qualified Data.ByteString.Char8 as BS8
-- import           System.Environment (getEnv)
import           Context (Ctx(..), readContextFromEnv)



main :: IO ()
main = do
  -- connectionStr <- getEnv "DBCONN"
  ctx <- readContextFromEnv
  -- let connectionStr = conn ctx
  let migrationDir = MigrationDirectory "db/migrations/"
  let con = conn ctx
  initResult <- withTransaction con $ runMigration $
    MigrationContext MigrationInitialization False con
  case initResult of
    MigrationError _ -> do
      putStrLn "failed to run intialization"
      print initResult
    MigrationSuccess -> do
      migrationResult <- withTransaction con $ runMigration $
        MigrationContext migrationDir True con
      print migrationResult
