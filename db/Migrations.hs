import           Database.PostgreSQL.Simple           (withTransaction)
import           Database.PostgreSQL.Simple.Migration (MigrationCommand (..),
                                                       MigrationContext (..),
                                                       MigrationResult (..),
                                                       runMigration)
import           Context                              (Ctx (..),
                                                       readContextFromEnv)
main :: IO ()
main = do
  ctx <- readContextFromEnv
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
