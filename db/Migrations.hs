import           Context                              (Ctx (..),conn, readContextFromEnv)
import Squeal.PostgreSQL
import Squeal.Schema
import Squeal.Migration.V1 (initMigration)

{-
 There needs to be some level of error reporting here?
 -}
main :: IO ()
main = do
  ctx <- readContextFromEnv
  let con = conn ctx
  withConnection (connStr ctx) $
    define initMigration
