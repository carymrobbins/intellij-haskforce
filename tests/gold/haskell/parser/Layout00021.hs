module Layout00021 where

runGhcModT opt action = do
    (f $ do action)

runGhcModT' a = do
  return res
