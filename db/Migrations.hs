import           Context                              (connStr, readContextFromEnv)
import Squeal.PostgreSQL
import Squeal.Migration.V1 (initMigration)

{-
 There needs to be some level of error reporting here?
 -}
main :: IO ()
main = do
  ctx <- readContextFromEnv
  withConnection (connStr ctx) $
    define initMigration
