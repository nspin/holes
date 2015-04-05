{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Holes.Words
    (
    ) where

import Control.Lens
import Data.Bits
import Data.Holes
import Data.Ix

deriving instance Bits       h => Bits       (Hole h)
deriving instance Bounded    h => Bounded    (Hole h)
deriving instance Enum       h => Enum       (Hole h)
deriving instance Eq         h => Eq         (Hole h)
deriving instance FiniteBits h => FiniteBits (Hole h)
deriving instance Integral   h => Integral   (Hole h)
deriving instance Ix         h => Ix         (Hole h)
deriving instance Num        h => Num        (Hole h)
deriving instance Ord        h => Ord        (Hole h)
deriving instance Real       h => Real       (Hole h)

deriving instance (Bounded (a h), Bounded (b h)) => Bounded (Holes a b h)
deriving instance (Eq      (a h), Eq      (b h)) => Eq      (Holes a b h)
deriving instance (Ix      (a h), Ix      (b h)) => Ix      (Holes a b h)
deriving instance (Ord     (a h), Ord     (b h)) => Ord     (Holes a b h)

-- instance ( Num (a h), Num (b h)
--          , Bounded (a h), Bounded (b h)
--          , Bits (a h), Bits (b h)
--          ) => Num (Holes a b h)

instance ( Index (a h) ~ Int,
           Index (b h) ~ Int,
           Ixed (a h),
           Ixed (b h),
           IxValue (a h) ~ h,
           IxValue (b h) ~ h,
          FiniteBits (a h), FiniteBits (b h),
          Bits (a h), Bits (b h),
          Traversable a, Traversable b,
          Bits h
         ) => Bits (Holes a b h) where
    (Holes a b) .&. (Holes c d) = Holes (a .&. c) (b .&. d)
    (Holes a b) .|. (Holes c d) = Holes (a .|. c) (b .|. d)
    xor (Holes a b) (Holes c d) = Holes (xor a c) (xor b d)
    complement (Holes a b) = Holes (complement a) (complement b)
    shift = undefined
    rotate = undefined
    bitSize = finiteBitSize
    bitSizeMaybe = Just . finiteBitSize
    isSigned _ = False
    testBit a i = testBit (a ^. ix (quot i 8)) i
    bit i = minBound & ix q .~ bit r
      where (q, r) = quotRem i 8
    popCount (Holes a b) = popCount a + popCount b

instance (Traversable a, Traversable b, FiniteBits h) => FiniteBits (Holes a b h) where
    finiteBitSize = sumOf traverse . fmap finiteBitSize

-- instance (IsHoles a, IsHoles b) => Num (Holes a b) where
--     (Holes a b) + (Holes c d) = Holes e f where
--         e = a + c
--         f = b + d + if e < a then 1 else 0
--     (Holes a b) * (Holes c d) = undefined
--     negate = (+ 1) . complement
--     abs = id
--     signum 0 = 0
--     signum _ = 1
--     fromInteger z = Holes (fromInteger r) (fromInteger q) where
--         (q, r) = quotRem z (toInteger (maxBound :: a))

-- instance (IsHoles a, IsHoles b) => Integral (Holes a b) where
--     toInteger (Holes a b) = toInteger a + toInteger b * (256 ^ lengthOf octets a)
--     quotRem (Holes a b) (Holes c d) = undefined

-- instance (IsHoles a, IsHoles b) => Enum (Holes a b) where

--     toEnum n = if i > toInteger (maxBound :: (Holes a b))
--                then error ("toEnum on " ++ show n ++ ": out of bounds.")
--                else fromInteger i
--       where i = toInteger n

--     fromEnum a = if i > toInteger (maxBound :: Int)
--                  then error ("fromEnum on " ++ show a ++ ": out of bounds.")
--                  else fromInteger i
--       where i = toInteger a

--     succ (Holes a b) = if a == maxBound
--                         then if b == maxBound
--                              then succError "(IsHoles a, IsHoles b) => Holes a b"
--                              else Holes minBound (succ b)
--                         else Holes (succ a) b

--     pred (Holes a b) = if a == minBound
--                         then if b == minBound
--                              then predError "(IsHoles a, IsHoles b) => Holes a b"
--                              else Holes maxBound (pred b)
--                         else Holes (pred a) b

--     enumFrom a = enumFromTo a maxBound
--     enumFromThen a b = enumFromThenTo a b (if a < b then maxBound else minBound)

--     enumFromTo (Holes a b) (Holes c d) =
--         case compare b d of
--             LT -> []
--             EQ -> map (flip Holes b) [a..c]
--             GT -> map (flip Holes b) [a..maxBound]
--                ++ (flip Holes <$> enumFromTo b d <*> [minBound..maxBound])
--                ++ map (flip Holes b) [minBound..c]

-- --     enumFromThenTo (Holes a b) (Holes c d) (Holes e f) =
-- --         case (if up then id else opp) (compare b d) of
-- --             LT -> []
-- --             EQ -> map (flip Holes b) [a..c]
-- --             GT -> map (flip Holes b) [a..maxBound]
-- --                ++ (flip Holes <$> enumFromTo b d <*> [minBound..maxBound])
-- --                ++ map (flip Holes b) [minBound..c]
-- --       where
-- --         up = case compare b d of
-- --             LT -> False
-- --             EQ -> a < c
-- --             GT -> True
-- --         start = if up then minBound else maxBound
-- --         end = if up then maxBound else minBound
-- --         opp LT = GT
-- --         opp EQ = EQ
-- --         opp GT = LT

-- instance (IsHoles a, IsHoles b) => Real (Holes a b) where
--     toRational = (% 1) . toInteger

