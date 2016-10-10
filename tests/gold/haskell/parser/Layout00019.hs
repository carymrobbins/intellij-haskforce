module Layout00019 where

f x =  maybeEP (\ids -> do
                  layoutList pts' $ sepInstFunBinds ids
                  ) mids
