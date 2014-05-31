{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
module Kind00002 where

data Term a where
      Lit    :: Int -> Term Int
      Succ   :: Term Int -> Term Int
      IsZero :: Term Int -> Term Bool
      If     :: Term Bool -> Term a -> Term a -> Term a
      Pair   :: Term a -> Term b -> Term (a,b)

data Counter a = forall self. NewCounter
    { _this    :: self
    , _inc     :: self -> self
    , _display :: self -> IO ()
    , tag      :: a
    }

setTag :: Counter a -> a -> Counter a
setTag obj t = obj{ tag = t }

class Monad m => MonadState s m | m -> s where
  get :: m s

data family Array :: * -> *

class Collects ce where
  type Elem2 ce :: *

type family F a :: *
type instance F Int = Bool
type instance F Float = Char
