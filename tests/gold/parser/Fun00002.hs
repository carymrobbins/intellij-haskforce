{-# LANGUAGE GADTs #-}
module Fun00002 where

-- | Type and size information of a Feldspar program
data Info a
  where
    Info
      :: (Show (Size a), Lattice (Size a))
      => { infoType   :: TypeRep a
         , infoSize   :: Size a
         , infoVars   :: VarInfo
         , infoSource :: SourceInfo
         }
      -> Info a
