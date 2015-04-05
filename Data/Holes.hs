{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Rank2Types #-}

module Data.Holes
    (
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.Bits
import           Data.Data
import           Data.Traversable
import           Data.Ix
import           Data.List
import           Data.Monoid
import           Data.Ratio
import           Data.Word (Word8)
import           GHC.Enum

type H1  = Hole
type H2  = Holes H1 H1
type H4  = Holes H2 H2
type H8  = Holes H4 H4
type H16 = Holes H8 H8

-------------------------------
-- HOLY
-------------------------------

class (Traversable a) => Holy a where
    type WrapWith w a :: * -> *

-------------------------------
-- HOLE
-------------------------------

newtype Hole h = Hole { inside :: h }
  deriving (Eq, Ord, Bounded, Read, Show, Ix)

instance Functor Hole where
    fmap = fmapDefault

instance Foldable Hole where
    foldMap = foldMapDefault

instance Traversable Hole where
    traverse f (Hole h) = Hole <$> f h

instance Each (Hole h) (Hole h') h h' where
   each = traverse

type instance Index (Hole h) = Int
type instance IxValue (Hole h) = h
instance Ixed (Hole h) where

-- instance Holy Hole where
--     type WrapWith w Hole = w Hole

-------------------------------
-- HOLES
-------------------------------

data Holes a b c = Holes (a c) (b c)
  deriving (Eq, Ord, Bounded, Read, Show, Ix)

instance (Holy a, Holy b) => Functor (Holes a b) where
    fmap = fmapDefault

instance (Holy a, Holy b) => Foldable (Holes a b) where
    foldMap = foldMapDefault

instance (Holy a, Holy b) => Traversable (Holes a b) where
    traverse f (Holes a b) = Holes <$> traverse f a <*> traverse f b

instance (Holy a, Holy b) => Each (Holes a b h) (Holes a b h') h h' where
    each = traverse

type instance Index (Holes a b h) = Int
type instance IxValue (Holes a b h) = h
instance Ixed (Holes a b h) where

-- instance (Holy a, Holy b) => Holy (Holes a b)  where
--     type WrapWith w (Holes a b) = Holes (w a) (w b)

-------------------------------
-- GENERAL
-------------------------------

countHoles :: (Holy a, Num b) => a h -> b
countHoles = sumOf traverse . fmap (const 1)

-- flatten :: (Holy a, Holy b, Holy c) => Holes a b c -> Holes _ _ _


-- ========================================================================
-- ========================================================================
-- ========================================================================

class ( Bounded w
      , Enum w
      , Eq w
      , Integral w
      , Num w
      , Ord w
      , Read w
      , Real w
      , Show w
      , Ix w
      , FiniteBits w
      ) => Wordy w where

newtype AWord a w = AWord { getAWord :: a w }

