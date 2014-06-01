{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Fun00010 (
    Maybe(..)
  , f
  , Maybe
  , Permissions(
      Permissions,
      readable,       -- :: Permissions -> Bool
      writable,       -- :: Permissions -> Bool
      executable,     -- :: Permissions -> Bool
      searchable      -- :: Permissions -> Bool
    )
  , module Data.GraphViz.Attributes.Colors
  ) where

f (Roc{url = url}) = id
f (Rac{..})        =  fun
f (Ruc{a})         = a