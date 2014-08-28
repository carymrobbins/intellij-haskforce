module Layout00021 where

runGhcModT :: IOish m
           => Options
           -> GhcModT m a
           -> m (Either GhcModError a, GhcModLog)
runGhcModT opt action = do
    env <- liftBase $ newGhcModEnv opt =<< getCurrentDirectory
    first (fst <$>) <$> (runGhcModT' env defaultState $ do
        dflags <- getSessionDynFlags
        defaultCleanupHandler dflags $ do
            initializeFlagsWithCradle opt (gmCradle env)
            action)

runGhcModT' :: IOish m
           => GhcModEnv
           -> GhcModState
           -> GhcModT m a
           -> m (Either GhcModError (a, GhcModState), GhcModLog)
runGhcModT' r s a = do
  (res, w') <-
      flip runReaderT r $ runJournalT $ runErrorT $
        runStateT (unGhcModT $ initGhcMonad (Just ghcLibDir) >> a) s
  return (res, w')
