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
	, Vec1(..), Vec2(..), Vec3(..), Vec4(..)
	, Vec1f, Vec2f, Vec3f, Vec4f
	, Vec1d, Vec2d, Vec3d, Vec4d
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
	, Mul(..)
	, Quaternion(..)
	) where

import Flaw.Math.Internal

genVecClasses
genVecDatas
genMatDatas
genMuls
genSynonyms

-- | Quaternion type.
newtype Quaternion a = Quaternion (Vec4 a)
