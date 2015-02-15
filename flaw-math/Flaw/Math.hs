{-|
Module: Flaw.Math
Description: Math.
License: MIT
-}

{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Flaw.Math
	( maxVecDimension
	, vecComponents
	, Vec(..)
	, VecX(..), VecY(..), VecZ(..), VecW(..)
	, SwizzleVec1(..), SwizzleVec2(..), SwizzleVec3(..), SwizzleVec4(..)
	, Vec1(..), Vec2(..), Vec3(..), Vec4(..)
	, Vec1f, Vec2f, Vec3f, Vec4f
	, Vec1d, Vec2d, Vec3d, Vec4d
	, Vec1i, Vec2i, Vec3i, Vec4i
	, Dot(..)
	, Cross(..)
	, Mat(..)
	, Mat1x1(..), Mat1x2(..), Mat1x3(..), Mat1x4(..)
	, Mat2x1(..), Mat2x2(..), Mat2x3(..), Mat2x4(..)
	, Mat3x1(..), Mat3x2(..), Mat3x3(..), Mat3x4(..)
	, Mat4x1(..), Mat4x2(..), Mat4x3(..), Mat4x4(..)
	, Mat1x1f, Mat1x2f, Mat1x3f, Mat1x4f
	, Mat2x1f, Mat2x2f, Mat2x3f, Mat2x4f
	, Mat3x1f, Mat3x2f, Mat3x3f, Mat3x4f
	, Mat4x1f, Mat4x2f, Mat4x3f, Mat4x4f
	, Mat1x1d, Mat1x2d, Mat1x3d, Mat1x4d
	, Mat2x1d, Mat2x2d, Mat2x3d, Mat2x4d
	, Mat3x1d, Mat3x2d, Mat3x3d, Mat3x4d
	, Mat4x1d, Mat4x2d, Mat4x3d, Mat4x4d
	, Mat1x1i, Mat1x2i, Mat1x3i, Mat1x4i
	, Mat2x1i, Mat2x2i, Mat2x3i, Mat2x4i
	, Mat3x1i, Mat3x2i, Mat3x3i, Mat3x4i
	, Mat4x1i, Mat4x2i, Mat4x3i, Mat4x4i
	, Mul(..)
	, Quaternion(..)
	) where

import Flaw.Math.Internal

genVecClasses
genSwizzleVecClasses
genVecDatas
genMatDatas
genMuls
genSynonyms

-- | Quaternion type.
newtype Quaternion a = Quaternion (Vec4 a)

-- | Cross.
instance Num a => Cross (Vec3 a) where
	cross (Vec3 ax ay az) (Vec3 bx by bz) = Vec3
		(ay * bz - by * az)
		(az * bx - ax * bz)
		(ax * by - ay * bx)
