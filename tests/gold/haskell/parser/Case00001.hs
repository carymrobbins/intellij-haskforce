module Case00001 where

-- Ensure broken case expressions don't break the parser.

connect :: Maybe String -> IO Pool
connect ehost = do
        etcd  <- case ehost of
                    Just h -> createClient[h]

connect' ehost = do
    etcd  <- case ehost of
              Just
