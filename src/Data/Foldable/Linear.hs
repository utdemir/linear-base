{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module provides
module Data.Foldable.Linear
  ( Foldable(..)
  )
  where

class Foldable t where
  foldr :: (a #-> b #-> b) -> b #-> t a #-> b

