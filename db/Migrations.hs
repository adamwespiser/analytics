import Database.PostgreSQL.Simple (connectPostgreSQL, withTransaction)
import Database.PostgreSQL.Simple.Migration (
  runMigration
  , MigrationContext(..)
  , MigrationCommand(..)
  , MigrationResult(..)
  )

import qualified Data.ByteString.Char8 as BS8
import System.Environment (getEnv)

main :: IO ()
main = do
  connectionStr <- getEnv "DBCONN"
  let migrationDir = MigrationDirectory "db/migrations/"
  let initializeTables = "db/migrations/intialize_tables.sql"
  con <- connectPostgreSQL (BS8.pack connectionStr)
  initResult <- withTransaction con $ runMigration $
    MigrationContext MigrationInitialization False con
  case initResult of
    MigrationError _ -> do
      print "failed to run intialization"
      print initResult
    MigrationSuccess -> do
      migrationResult <- withTransaction con $ runMigration $
        MigrationContext migrationDir True con
      print migrationResult
