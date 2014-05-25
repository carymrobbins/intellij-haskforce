{-# LANGUAGE GADTs #-}
module Fun00003 where

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

instance Show (Info a)
  where
    show i@(Info {}) = show (infoType i) ++ szStr ++ srcStr
      where
        szStr = case show (infoSize i) of
          "AnySize" -> ""
          str       -> " | " ++ str

        srcStr = case infoSource i of
          ""  -> ""
          src -> " | " ++ src
