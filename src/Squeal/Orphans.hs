module Squeal.Orphans where

import           Control.Monad.Catch       (MonadCatch (..), MonadMask (..),
                                            MonadThrow (..))
import           Control.Monad.Trans.Class (lift)
import           Squeal.PostgreSQL


instance (MonadThrow m, db0 ~ db1)
  => MonadThrow (PQ db0 db1 m) where
  throwM = lift . throwM

instance (MonadCatch m, db0 ~ db1)
  => MonadCatch (PQ db0 db1 m) where
  catch (PQ m) f = PQ $ \k -> m k `catch` \e -> unPQ (f e) k

instance (MonadMask m, db0 ~ db1)
  => MonadMask (PQ db0 db1 m) where
  mask a = PQ $ \e -> mask $ \u -> unPQ (a $ q u) e
    where q u (PQ b) = PQ (u . b)
  uninterruptibleMask a =
    PQ $ \k -> uninterruptibleMask $ \u -> unPQ (a $ q u) k
      where q u (PQ b) = PQ (u . b)

  generalBracket acquire release use = PQ $ \k ->
    K <$> generalBracket
      (unK <$> unPQ acquire k)
      (\resource exitCase -> unK <$> unPQ (release resource exitCase) k)
      (\resource -> unK <$> unPQ (use resource) k)

