{-# LANGUAGE UndecidableInstances #-}
module Types (
   App
 , MonadAuth
 , withAuth
 , HasContext
 , getContext
 , getPool
 , runAppInTransaction
) where

import           Context                    (Ctx (..))
import           Control.Monad.Catch        hiding (Handler)
import           Control.Monad.Extra        (ifM)
import           Control.Monad.IO.Class
import           Control.Monad.Reader       (MonadReader, ask)
import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Data.Text                  as T
import           Servant
import           Squeal.Orphans             ()
import           Squeal.PostgreSQL
import           Squeal.Schema


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
type App = AppT Ctx (PQ DB DB IO)

instance MonadTrans (AppT r) where
  lift = AppT . lift

runAppInTransaction :: Ctx -> App a -> IO a
runAppInTransaction ctx = usingConnectionPool (conn ctx) . runApp ctx
  where
    runApp :: Ctx -> App a -> PQ DB DB IO a
    runApp cfg = flip runReaderT cfg . unAppT

instance (schemas ~ DB, MonadPQ schemas m) => MonadPQ schemas (AppT r m) where
  executeParams q = lift . executeParams q
  executePrepared q = lift . executePrepared q
  executePrepared_ q = lift . executePrepared_ q

class (Monad m, MonadThrow m, MonadReader Ctx m) => MonadAuth m where
  withAuth :: Maybe T.Text -> m a -> m a

instance (schemas ~ DB) => MonadAuth (AppT Ctx (PQ schemas schemas IO))  where
  withAuth auth f =
    ifM (isCorrectAuth auth) f
        (lift $ throwM $ err403)
    where
      isCorrectAuth :: HasContext m => Maybe T.Text -> m Bool
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
