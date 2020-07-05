{-# LANGUAGE UndecidableInstances #-}
module Types (
   AppM
 , App
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
 , runAppInTransaction
) where

import           ApiTypes                                 (Event (..),
                                                           PageView (..),
                                                           UserSession (..))
import           Context                                  (Ctx (..))
import           Control.Monad.Extra                      (ifM)
import           Control.Monad.IO.Class                   (liftIO)
import           Control.Monad.Trans.Class                (lift, MonadTrans)
import           Control.Monad.Trans.Except               (throwE)
import           Control.Monad.Trans.Reader               (ReaderT, runReaderT)
import Control.Monad.Reader (MonadReader, ask)
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
import Squeal.Schema
import Squeal.Orphans ()
import Control.Monad.IO.Class
import Control.Monad.Catch hiding (Handler)


newtype AppT r m a = AppT { unAppT :: ReaderT r m a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader r
    , MonadIO
    , MonadCatch
    , MonadThrow
    , MonadMask
    )
type AppT' = AppT Ctx
type App = AppT Ctx (PQ DB DB IO)

instance MonadTrans (AppT r) where
  lift = AppT . lift

type AppM ctx = ReaderT ctx Handler

runApp :: Ctx -> App a -> PQ DB DB IO a
runApp cfg = flip runReaderT cfg . unAppT

runAppInTransaction :: Ctx -> App a -> IO a
runAppInTransaction ctx = usingConnectionPool (conn ctx) . runApp ctx

instance (schemas ~ DB, MonadPQ schemas m) => MonadPQ schemas (AppT r m) where
  executeParams q = lift . executeParams q
  executePrepared q = lift . executePrepared q
  executePrepared_ q = lift . executePrepared_ q

----------------------------------
class (Monad m, MonadThrow m, MonadReader Ctx m) => MonadAuth m where
  withAuth :: Maybe T.Text -> m a -> m a

instance (schemas ~ DB) => MonadAuth (AppT Ctx (PQ schemas schemas IO))  where
  withAuth auth f =
    ifM (isCorrectAuth auth) f
        (lift $ throwM $ TServerError 403)
    where
      isCorrectAuth :: (MonadReader Ctx m) => Maybe T.Text -> m Bool
      isCorrectAuth auth' = do
        Ctx{..} <- ask
        pure $ auth' == Just apiKey

class HasContext m => HasDbConn m where
  getPool :: m (Pool (K Connection DB))
instance (schemas ~ DB) => HasDbConn (AppT Ctx (PQ schemas schemas IO))  where
  getPool = conn <$> getContext

class (MonadReader Ctx m, Monad m) => HasContext m where
  getContext :: m Ctx
instance (schemas ~ DB) => HasContext (AppT Ctx (PQ schemas schemas IO))  where
  getContext = ask

class (Monad m, HasDbConn m) => MonadDb m where
  insertUserSession :: m UUID.UUID
  fetchUserSession :: m UserSession
  insertPageView :: PageView -> m ()
  insertEvent :: Event -> m ()
instance (schemas ~ DB) => MonadDb (AppT Ctx (PQ schemas schemas IO))  where
  insertUserSession = undefined
  fetchUserSession = undefined
  insertPageView _ = undefined
  insertEvent _ = undefined


data MyException = TServerError Integer
    deriving Show
instance Exception MyException

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

