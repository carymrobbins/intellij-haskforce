module Import00003 where

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
import Data.Typeable (Typeable)
#else
import Data.Typeable (Typeable,Typeable1,mkTyCon3,mkTyConApp,typeOf)
#endif
