module Types (
   AppM
 , MonadAuth
 , withAuth
 , MonadDb
 , insertUserSession
 , fetchUserSession
 , insertPageView
 , insertEvent
 , HasContext
 , getContext
 , getPool
) where

import           ApiTypes                                 (Event (..),
                                                           PageView (..),
                                                           UserSession (..))
import           Context                                  (Ctx (..))
import           Control.Monad.Extra                      (ifM)
import           Control.Monad.IO.Class                   (liftIO)
import           Control.Monad.Trans.Class                (lift)
import           Control.Monad.Trans.Except               (throwE)
import           Control.Monad.Trans.Reader               (ReaderT, ask)
import Control.Monad.Reader (MonadReader)
import qualified Data.Text                                as T
import qualified Data.UUID.Types                          as UUID (UUID)
{-
import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import qualified Database.Beam.Postgres                   as Pg
-}

import           Servant
-- import           Servant.Server                           (err403)
import           Squeal.Schema                            (DB)
import Squeal.PostgreSQL


type AppM ctx = ReaderT ctx Handler

class (Monad m) => MonadAuth m where
  withAuth :: Maybe T.Text -> m a -> m a

instance MonadAuth (AppM Ctx) where
  withAuth auth f =
    ifM (isCorrectAuth auth) f
        (lift $ Handler $ throwE err403)
    where
      isCorrectAuth :: Maybe T.Text -> AppM Ctx Bool
      isCorrectAuth auth' = do
        Ctx{..} <- getContext
        pure $ auth' == Just apiKey

class HasContext m => HasDbConn m where
  getPool :: m (Pool (K Connection DB))
instance HasDbConn (AppM Ctx) where
  getPool = conn <$> getContext

class (MonadReader Ctx m, Monad m) => HasContext m where
  getContext :: m Ctx
instance HasContext (AppM Ctx) where
  getContext = ask

class (Monad m, HasDbConn m) => MonadDb m where
  insertUserSession :: m UUID.UUID
  fetchUserSession :: m UserSession
  insertPageView :: PageView -> m ()
  insertEvent :: Event -> m ()
instance MonadDb (AppM Ctx)  where
  insertUserSession = undefined
  fetchUserSession = undefined
  insertPageView _ = undefined
  insertEvent _ = undefined

{-
instance MonadDb (AppM Ctx)  where
  insertUserSession conn = liftIO $ Pg.runBeamPostgresDebug putStrLn conn  $ do
    insertValue <-
      runInsertReturningList $ insert (dbUserSession analyticsDb) $ insertExpressions [UserSessionDB B.default_ Pg.now_]
    pure insertValue
  fetchUserSession _ = liftIO $ return $ UserSession UUID.nil
  insertPageView conn pageview = liftIO $ Pg.runBeamPostgresDebug putStrLn conn $ runInsert $
    insert (dbPageView analyticsDb) $ insertExpressions [convertToDb pageview]
  insertEvent conn event = liftIO $ Pg.runBeamPostgresDebug putStrLn conn $ runInsert $
    insert (dbEvents analyticsDb) $ insertExpressions [convertToDb event]
-}

