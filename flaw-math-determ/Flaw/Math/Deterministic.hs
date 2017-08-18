{-|
Module: Flaw.Math.Deterministic
Description: Deterministic math via SSE2. Only scalar SSE operations are used, so vectors and matrices are inefficient.
License: MIT
-}

{-# LANGUAGE DeriveGeneric, TemplateHaskell, TypeFamilies #-}

module Flaw.Math.Deterministic
	( DetermFloat(..)
	, DetermFloat1, DetermFloat2, DetermFloat3, DetermFloat4
	, DetermFloat3x3, DetermFloat3x4, DetermFloat4x4
	) where

import Data.Int
import Data.Ratio
import Data.Word

import Flaw.Math
import Flaw.Math.Internal

newtype DetermFloat = DetermFloat Word32

instance Eq DetermFloat where
	DetermFloat a == DetermFloat b = determ_eq_float a b
	{-# INLINE (==) #-}
	DetermFloat a /= DetermFloat b = determ_neq_float a b
	{-# INLINE (/=) #-}

instance Ord DetermFloat where
	DetermFloat a < DetermFloat b = determ_lt_float a b
	{-# INLINE (<) #-}
	DetermFloat a <= DetermFloat b = determ_le_float a b
	{-# INLINE (<=) #-}
	DetermFloat a > DetermFloat b = determ_gt_float a b
	{-# INLINE (>) #-}
	DetermFloat a >= DetermFloat b = determ_ge_float a b
	{-# INLINE (>=) #-}

instance Num DetermFloat where
	DetermFloat a + DetermFloat b = DetermFloat (determ_add_float a b)
	{-# INLINE (+) #-}
	DetermFloat a - DetermFloat b = DetermFloat (determ_subtract_float a b)
	{-# INLINE (-) #-}
	DetermFloat a * DetermFloat b = DetermFloat (determ_multiply_float a b)
	{-# INLINE (*) #-}
	negate (DetermFloat a) = DetermFloat (determ_negate_float a)
	{-# INLINE negate #-}
	abs (DetermFloat a) = DetermFloat (determ_abs_float a)
	{-# INLINE abs #-}
	signum = error "no signum for DetermFloat"
	fromInteger = DetermFloat . determ_float_from . fromInteger
	{-# INLINE fromInteger #-}

instance Fractional DetermFloat where
	DetermFloat a / DetermFloat b = DetermFloat (determ_divide_float a b)
	{-# INLINE (/) #-}
	fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
	{-# INLINE fromRational #-}

instance Real DetermFloat where
	toRational = error "no toRational for DetermFloat"

instance RealFrac DetermFloat where
	properFraction = error "no properFraction for DetermFloat"
	{-# INLINE properFraction #-}
	truncate (DetermFloat a) = fromIntegral $ determ_trunc_float_int a
	{-# INLINE truncate #-}
	round = error "no round for DetermFloat"
	ceiling (DetermFloat a) = fromIntegral $ determ_trunc_float_int $ determ_ceil_float a
	{-# INLINE ceiling #-}
	floor (DetermFloat a) = fromIntegral $ determ_trunc_float_int $ determ_floor_float a
	{-# INLINE floor #-}

instance Floating DetermFloat where
	pi = DetermFloat 0x40490fdb -- has to be the same as in C
	{-# INLINE pi #-}
	exp = error "no exp for DetermFloat"
	log = error "no log for DetermFloat"
	sqrt (DetermFloat a) = DetermFloat (determ_sqrt_float a)
	{-# INLINE sqrt #-}
	(**) = error "no ** for DetermFloat"
	logBase = error "no logBase for DetermFloat"
	sin (DetermFloat a) = DetermFloat (determ_sin_float a)
	{-# INLINE sin #-}
	cos (DetermFloat a) = DetermFloat (determ_cos_float a)
	{-# INLINE cos #-}

instance Show DetermFloat where
	showsPrec p (DetermFloat a) = showsPrec p (determ_float_to a)
	{-# INLINE showsPrec #-}

foreign import ccall unsafe determ_float_from :: Float -> Word32
foreign import ccall unsafe determ_float_to :: Word32 -> Float
foreign import ccall unsafe determ_eq_float  :: Word32 -> Word32 -> Bool
foreign import ccall unsafe determ_neq_float  :: Word32 -> Word32 -> Bool
foreign import ccall unsafe determ_lt_float  :: Word32 -> Word32 -> Bool
foreign import ccall unsafe determ_le_float  :: Word32 -> Word32 -> Bool
foreign import ccall unsafe determ_gt_float  :: Word32 -> Word32 -> Bool
foreign import ccall unsafe determ_ge_float  :: Word32 -> Word32 -> Bool
foreign import ccall unsafe determ_add_float  :: Word32 -> Word32 -> Word32
foreign import ccall unsafe determ_subtract_float :: Word32 -> Word32 -> Word32
foreign import ccall unsafe determ_multiply_float :: Word32 -> Word32 -> Word32
foreign import ccall unsafe determ_divide_float :: Word32 -> Word32 -> Word32
foreign import ccall unsafe determ_negate_float :: Word32 -> Word32
foreign import ccall unsafe determ_abs_float :: Word32 -> Word32
foreign import ccall unsafe determ_floor_float :: Word32 -> Word32
foreign import ccall unsafe determ_ceil_float :: Word32 -> Word32
-- foreign import ccall unsafe determ_trunc_float :: Word32 -> Word32
foreign import ccall unsafe determ_trunc_float_int :: Word32 -> Int32
foreign import ccall unsafe determ_sqrt_float :: Word32 -> Word32
foreign import ccall unsafe determ_sin_float :: Word32 -> Word32
foreign import ccall unsafe determ_cos_float :: Word32 -> Word32

mathTypeVectorizedDecls ''DetermFloat "DetermFloat"
