module Layout00014 where

instance Indexed (Pull sh a) where
    Pull ixf _ ! i = ixf i
