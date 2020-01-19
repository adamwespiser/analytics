{-# LANGUAGE OverloadedStrings #-}

module Helpers (withDB) where

import Protolude
import qualified Data.ByteString.Char8       as BSC
import           Data.String.Conversions     (cs)
import           Database.Postgres.Temp      (DB (..), defaultOptions)
import qualified Database.Postgres.Temp      as PG
import qualified Database.Beam.Postgres      as Pg
import           Data.Maybe                  (fromMaybe)

import           Database.PostgreSQL.Simple.Options (Options (..))
import           System.IO                          (IOMode (WriteMode), openFile)
import           Test.Hspec
import           Database.PostgreSQL.Simple (Query, withTransaction, execute_)
import           Database.PostgreSQL.Simple.Types (Query(..))
import           System.Environment (getEnv)
import           Database.PostgreSQL.Simple.Migration (
  runMigration
  , MigrationContext(..)
  , MigrationCommand(..)
  , MigrationResult(..)
  )
import            Context (Ctx(..), readContextFromEnvWithConnStr)
import qualified Data.Text as T
--- Setup and teardown helpers
---
pg_tables = ["user_session", "page_view", "events"]

data DBLogging = VERBOSE | SILENT deriving Read

data TestType = Local | Travis deriving Read

withDB :: SpecWith (IO (), Ctx) -> Spec
withDB = beforeAll getDatabase . afterAll fst . after (truncateDb . snd)
 where
  getDatabase :: IO (IO (), Ctx)
  getDatabase = getEnv "TEST_TYPE" >>= \case
    "Local" -> createTmpDatabase
    _ -> do -- "TRAVIS"
      let connStr = "postgresql://postgres@localhost/travis_ci_test"
      config <- readContextFromEnvWithConnStr $ T.pack connStr
      migrateDB $ conn config
      pure (pure (), config)

  createTmpDatabase :: IO (IO (), Ctx)
  createTmpDatabase = do
    verbosity <- getEnv "DBLOGGING"
    (db, cleanup) <- startDb verbosity
    pguser <- getEnv "PGUSER"
    config <- readContextFromEnvWithConnStr $ toConnectionString pguser db
    return (cleanup, config)

  -- https://stackoverflow.com/questions/5342440/reset-auto-increment-counter-in-postgres
  truncateDb :: Ctx -> IO ()
  truncateDb config = execute_ (conn config) query_statment >> pure ()
   where
    query_statment =  Query $ BSC.pack $ T.unpack truncateStatement :: Query
    truncateStatement = "TRUNCATE TABLE " <> T.intercalate ", " pg_tables <> " RESTART IDENTITY CASCADE"
    --truncateTables = rawExecute  truncateStatement []

  toConnectionString :: String -> DB -> T.Text
  toConnectionString defaultUser DB {..} =
    "postgresql://" <> user <> "@" <> host <> ":" <> Protolude.show port <> "/" <> cs oDbname
   where
    Options {..} = options
    host = cs $ fromMaybe "localhost" oHost
    user = cs $ fromMaybe defaultUser oUser

  startDb :: String -> IO (DB, IO ())
  startDb verbosity = mask $ \restore -> do
    Prelude.putStrLn "startDB start"
    (outHandle, errHandle) <- case verbosity of
      "VERBOSE" -> pure (stdout, stderr)
      _ -> (,) <$> devNull <*> devNull -- "SILENT"
    db <- PG.startWithHandles PG.Localhost defaultOptions outHandle errHandle >>= either throwIO pure
    pguser <- getEnv "PGUSER"
    conn <- Pg.connectPostgreSQL $ BSC.pack $ T.unpack $ toConnectionString pguser db
    restore (migrateDB conn >>  Prelude.putStrLn "startDb end" >> pure (db, cleanup db))
      `onException` cleanup db
   where
    devNull = openFile "/dev/null" WriteMode
    cleanup = void . PG.stop

  migrateDB :: Pg.Connection -> IO ()
  migrateDB  con = do
    let migrationDir = MigrationDirectory "db/migrations"
    initResult <- withTransaction con $ runMigration $
      MigrationContext MigrationInitialization False con
    case initResult of
      MigrationError _ -> do
        Prelude.putStrLn "failed to run intialization"
        Prelude.print initResult
      MigrationSuccess -> do
        migrationResult <- withTransaction con $ runMigration $
          MigrationContext migrationDir True con
        Prelude.print migrationResult
