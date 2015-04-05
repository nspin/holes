{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Holes
    ( Hole(..)
    , Holes(..)
    ) where

import           Control.Lens hiding (holes)
import           Data.Traversable
import           Data.Monoid
import           Data.Ix
import           Data.Bits

newtype Hole h = Hole { inside :: h }
  deriving (Read, Show)

data Holes a b c = Holes (a c) (b c)
  deriving (Read, Show)

-------------------------------
-- HOLE
-------------------------------

instance Functor Hole where
    fmap = fmapDefault

instance Foldable Hole where
    foldMap = foldMapDefault

instance Traversable Hole where
    traverse f (Hole h) = Hole <$> f h

type instance Index (Hole h) = Int
type instance IxValue (Hole h) = h
instance Ixed (Hole h) where
    ix 0 f (Hole h) = Hole <$> f h
    ix _ _ hole = pure hole

-------------------------------
-- HOLES
-------------------------------

instance (Functor a, Functor b) => Functor (Holes a b) where
    fmap f (Holes a b) = Holes (fmap f a) (fmap f b)

instance (Foldable a, Foldable b) => Foldable (Holes a b) where
    foldMap f (Holes a b) = foldMap f a <> foldMap f b

instance (Traversable a, Traversable b) => Traversable (Holes a b) where
    traverse f (Holes a b) = Holes <$> traverse f a <*> traverse f b

type instance Index (Holes a b h) = Int
type instance IxValue (Holes a b h) = h
instance ( Index (a h) ~ Int
         , Index (b h) ~ Int
         , IxValue (a h) ~ h
         , IxValue (b h) ~ h
         , Ixed (a h)
         , Ixed (b h)
         , Traversable a
         , Traversable b
         ) => Ixed (Holes a b h) where
    ix i f (Holes a b) =
        if i < h
        then Holes <$> ix i f a <*> pure b
        else Holes a <$> ix (i - h) f b
      where h = holes a

-------------------------------
-- UTIL
-------------------------------

holes :: (Traversable a, Num b) => a h -> b
holes = sumOf traverse . (1 <$)

-------------------------------
-- SYNONYMS
-------------------------------

type H1  = Hole
type H2  = Holes H1 H1
type H4  = Holes H2 H2
type H8  = Holes H4 H4
type H16 = Holes H8 H8

