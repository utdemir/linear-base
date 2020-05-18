{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module provides pull arrays
--
-- Please import this module qualified for clarity and to avoid
-- name clashes.
--
-- == Example
--
-- > import Data.Array.Polarized
-- > import qualified Data.Array.Polarized.Push as Push
-- > import qualified Data.Array.Polarized.Pull as Pull
-- > import Data.Vector (Vector, (!), fromList)
-- > import qualified Prelude as P
-- >
-- > pullArrExample :: IO ()
-- > pullArrExample = do
-- >   x <- inputVectorX
-- >   y <- inputVectorY
-- >   z <- inputVectorZ
-- >   let x_pull = Pull.fromVector x
-- >   let y_pull = Pull.fromVector y
-- >   let z_pull = Pull.fromVector z
-- >   let nsum = normSumExample x_pull y_pull z_pull
-- >   putStrLn P.$ "norm sum is " ++ show nsum
-- >
-- > normSumExample :: Pull Int -> Pull Int -> Pull Int -> Double
-- > normSumExample x y z = norm (sumV (sumV x y) z)
-- >
-- > sumV :: Pull Int #-> Pull Int #-> Pull Int
-- > sumV = Pull.zipWith (Linear.+)
-- >
-- > norm :: Pull Int -> Double
-- > norm arr = sqrt P.$ fromIntegral P.$
-- >   Pull.foldr (Linear.+) 0 (Linear.fmap square arr)
-- >
-- > square :: Int #-> Int
-- > square i = fromDup (dup2 i)
-- >   where
-- >     fromDup :: (Int, Int) #-> Int
-- >     fromDup (i,j) = i Linear.* j
-- >
-- > -- | Query from environment
-- > inputVectorX :: IO (Vector Int)
-- > inputVectorX = return (fromList [1..100])
-- >
-- > -- | Query from environment
-- > inputVectorY :: IO (Vector Int)
-- > inputVectorY = return (fromList (map (\x -> (7 * (x+3)) `div` 11) [1..100]))
-- >
-- > -- | Query from environment
-- > inputVectorZ :: IO (Vector Int)
-- > inputVectorZ = return (fromList [negate i | i <- [1..100]])
--
module Data.Array.Polarized.Pull
  ( Array
  , zip, zipWith
  , append
  , make
  , foldr
  , foldMap
  , findLength
  , asList
  , singleton
  , fromFunction
  , fromVector
  , toVector
  , split
  , reverse
  )
  where

import Data.Array.Polarized.Pull.Internal
-- XXX: the data constructor Pull.Array could be used unsafely, so we don't
-- export it, instead exporting a collection of functions to manipulate
-- PullArrays
-- (eg one could use an element multiple times, if the constructor was
-- available)
-- TODO: the current collection is almost certainly not complete: it would be
-- nice if there was one (or a small number) of functions which characterise
-- PullArrays, but I'm not sure what they are
-- In particular, PullArrays are incredibly unfriendly in returned-value
-- position at the moment, moreso than they should be
import qualified Data.Functor.Linear as Data
import Prelude.Linear hiding (zip, zipWith, foldr, foldMap, reverse)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Unsafe.Linear as Unsafe

-- | Convert a pull array into a list.
asList :: Array a #-> [a]
asList = foldr (\x xs -> x:xs) []

-- | /!\ Partial! Only works if both arrays have the same length.
zipWith :: (a #-> b #-> c) -> Array a #-> Array b #-> Array c
zipWith f x y = Data.fmap (uncurry f) (zip x y)

-- | Fold a pull array into a monoid.
foldMap :: Monoid m => (a #-> m) -> Array a #-> m
foldMap f = foldr ((<>) . f) mempty

-- I'm fairly sure this can be used safely
-- | Convert a Vector to a pull array.
fromVector :: Vector a #-> Array a
fromVector = Unsafe.toLinear $ \v -> fromFunction (v Vector.!) (Vector.length v)
