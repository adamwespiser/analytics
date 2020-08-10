{-# LANGUAGE OverloadedStrings #-}

module Helpers (withDB) where

import qualified Data.ByteString.Char8              as BSC
import           Data.String.Conversions            (cs)
import           Database.Postgres.Temp             (DB (..), defaultOptions)
import qualified Database.Postgres.Temp             as PG
import           Protolude

import           Context                            (Ctx (..), CtxTest (..),
                                                     readContextFromEnv,
                                                     readContextFromEnvWithConnStr)
import qualified Control.Concurrent                 as C
import qualified Data.Text                          as T
import           Database.PostgreSQL.Simple         (Connection, Query,
                                                     connectPostgreSQL,
                                                     execute_)
import           Database.PostgreSQL.Simple.Options (Options (..))
import           Database.PostgreSQL.Simple.Types   (Query (..))
import qualified Network.Wai.Handler.Warp           as Warp
import           Server                             (app)
import           Squeal.Migration.V1
import           Squeal.PostgreSQL                  (renderSQL)
import           System.Environment                 (getEnv)
import           System.IO                          (BufferMode (..),
                                                     hSetBuffering)
import           Test.Hspec
--- Setup and teardown helpers
---
pgTables :: [Text]
pgTables = ["user_session", "page_view", "events"]

data DBLogging = VERBOSE | SILENT deriving Read

data TestType = Local | Travis deriving Read

withDB :: SpecWith (IO (), CtxTest) -> Spec
withDB = beforeAll getDbAndWarpServer
           . afterAll fst
           . afterAll (truncateDb . snd)
 where
  getDbAndWarpServer :: IO (IO (), CtxTest)
  getDbAndWarpServer = do
    (_, config) <- getDatabase
    ctx <- readContextFromEnv
    _ <- C.forkIO $ runWarpServer ctx
    -- Wait 2 seconds for the warp server to boot
    C.threadDelay 2000000
    pure (pure (), config)

  getDatabase :: IO (IO (), CtxTest)
  getDatabase = read @TestType <$> (getEnv "TEST_TYPE") >>= \case
    Local -> createTmpDatabase
    Travis -> do -- "TRAVIS"
      let connStr = "postgresql://postgres@localhost/travis_ci_test"
      config <- readContextFromEnvWithConnStr $ T.pack connStr
      migrateDB $ connT config
      pure (pure (), config)

  createTmpDatabase :: IO (IO (), CtxTest)
  createTmpDatabase = do
    verbosity <- read @DBLogging <$> getEnv "DBLOGGING"
    (db, cleanup) <- startDb verbosity
    pguser <- getEnv "PGUSER"
    config <- readContextFromEnvWithConnStr $ toConnectionString pguser db
    return (cleanup, config)

  -- https://stackoverflow.com/questions/5342440/reset-auto-increment-counter-in-postgres
  truncateDb :: CtxTest -> IO ()
  truncateDb config = execute_ (connT config) query_statment >> pure ()
   where
    query_statment =  Query $ BSC.pack $ T.unpack truncateStatement :: Query
    truncateStatement =
      "TRUNCATE TABLE " <> T.intercalate ", " pgTables <> " RESTART IDENTITY CASCADE"

  toConnectionString :: String -> DB -> T.Text
  toConnectionString defaultUser DB {..} =
    "postgresql://" <> user <> "@" <> host <> ":" <> Protolude.show port <> "/" <> cs oDbname
   where
    Options {..} = options
    host = cs $ fromMaybe "localhost" oHost
    user = cs $ fromMaybe defaultUser oUser

  startDb :: DBLogging -> IO (DB, IO ())
  startDb verbosity = mask $ \restore -> do
    (outHandle, errHandle) <- case verbosity of
      VERBOSE -> pure (stdout, stderr)
      SILENT  -> (,) <$> devNull <*> devNull
    db <- PG.startWithHandles PG.Localhost defaultOptions outHandle errHandle
            >>= either throwIO pure
    pguser <- getEnv "PGUSER"
    conn <- connectPostgreSQL $ BSC.pack $ T.unpack $ toConnectionString pguser db
    restore (migrateDB conn >> pure (db, cleanup db))
      `onException` cleanup db
   where
    devNull = openFile "/dev/null" WriteMode
    cleanup = void . PG.stop

  migrateDB :: Connection -> IO ()
  migrateDB  con =
    let migrationSql = Query $ renderSQL initMigration
     in void $ execute_ con migrationSql

    {-
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
     -}

  runWarpServer :: Ctx -> IO ()
  runWarpServer ctx = do
    let settings = Warp.defaultSettings
          & Warp.setPort (Context.port ctx)
          & Warp.setBeforeMainLoop
            (hPutStrLn stderr ("listening on port " ++ Protolude.show (Context.port ctx)))
          & Warp.setOnException
              (\req ex -> hPutStrLn stderr ("warp exception " ++ Protolude.show req ++ " " ++ Protolude.show ex ))
        serverThread app' = do
          hSetBuffering stdout LineBuffering
          Warp.runSettings settings $ app' ctx
    (C.forkIO $ serverThread $ app) >> pure ()

