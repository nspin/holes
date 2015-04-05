{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Holes
    (
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Lens.Indexed
import           Data.Bits
import           Data.Data
import           Data.Traversable
import           Data.Ix
import           Data.List
import           Data.Monoid
import           Data.Ratio
import           Data.Word (Word8)
import           GHC.Enum

newtype Hole h = Hole { inside :: h }
  deriving ( Eq, Ord, Bounded, Read, Show, Ix -- Normal
           , Enum, Real, Num, Bits, Integral, FiniteBits -- Generalized
           )

data Holes a b c = Holes (a c) (b c)
  deriving (Eq, Ord, Bounded, Read, Show, Ix)

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
        if i < holes
        then Holes <$> ix i f a <*> pure b
        else Holes a <$> ix (i - holes) f b
      where holes = countHoles a

-------------------------------
-- GENERAL
-------------------------------

countHoles :: (Traversable a, Num b) => a h -> b
countHoles = sumOf traverse . (1 <$)

-------------------------------
-- WRAPPABLE
-------------------------------

class Wrappable (a :: * -> *) where
    type Wrap (w :: (* -> *) -> * -> *) a :: * -> *

instance (Wrappable a, Wrappable b) => Wrappable (Holes a b)  where
    type Wrap w (Holes a b) = Holes (Wrap w a) (Wrap w b)

instance Wrappable Hole where
    type Wrap w Hole = w Hole

-------------------------------
-- BIGWORD
-------------------------------

newtype AsBigWord (a :: (* -> *)) h = AsBigWord { getBigWord :: (a h) }
  deriving ( Eq, Ord, Bounded, Read, Show, Ix -- Normal
           , Enum, Real, Num, Bits, Integral, FiniteBits -- Generalized
           )

instance (Num (a h), Num (b h)) => Num (Holes a b h) where
    (Holes a b) + (Holes c d) = 

-------------------------------
-- SYNONYMS
-------------------------------

type H1  = Hole
type H2  = Holes H1 H1
type H4  = Holes H2 H2
type H8  = Holes H4 H4
type H16 = Holes H8 H8

type W1  = Wrap AsBigWord H1
type W2  = Wrap AsBigWord H2
type W8  = Wrap AsBigWord H8
type W16 = Wrap AsBigWord H16

