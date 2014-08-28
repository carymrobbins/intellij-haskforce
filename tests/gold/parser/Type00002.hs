module Type00002 where

newtype GhcModT m a = GhcModT {
      unGhcModT :: StateT GhcModState
                     (ErrorT GhcModError
                       (JournalT GhcModLog
                         (ReaderT GhcModEnv m) ) ) a
    } deriving ( Functor
               , Applicative
               , Alternative
               , Monad
               , MonadPlus
               , MonadIO
#if DIFFERENT_MONADIO
               , Control.Monad.IO.Class.MonadIO
#endif
               , MonadReader GhcModEnv -- TODO: make MonadReader instance
                                       -- pass-through like MonadState
               , MonadWriter w
               , MonadError GhcModError
               )
