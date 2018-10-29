module Instance00002 where

newtype FooT m a = FooT { unFooT :: ReaderT Foo m a }
  deriving newtype (Functor, MonadIO, Applicative, Monad, MonadTrans, MonadThrow, MonadCatch, MonadLogger, MonadMask, MonadResource)

deriving newtype instance MonadBase IO m => MonadBase IO (FooT m)
deriving newtype instance MonadBaseControl IO m => MonadBaseControl IO (FooT m)
deriving newtype instance MonadError e m => MonadError e (FooT m)
