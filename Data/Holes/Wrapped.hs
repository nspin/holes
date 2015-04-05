{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Holes.Wrapped
    (
    ) where

import Data.Holes

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

-- newtype AsBigWord (a :: (* -> *)) h = AsBigWord { getBigWord :: (a h) }
--   deriving ( Eq, Ord, Bounded, Read, Show, Ix -- Normal
--            , Enum, Real, Num, Bits, Integral, FiniteBits -- Generalized
--            )

-- instance (Num (a h), Num (b h)) => Num (Holes a b h) where
--     (Holes a b) + (Holes c d) = 

