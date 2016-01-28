{-|
Module: Flaw.Visual
Description: Shader stuff.
License: MIT
-}

module Flaw.Visual
	( applyQuat
	, applyQuatOffset
	) where

import Flaw.Graphics.Program
import Flaw.Graphics.Program.Internal
import Flaw.Math

applyQuat :: (OfScalarType a, Quaternionized a) => Node (Vec4 a) -> Node (Vec3 a) -> Node (Vec3 a)
applyQuat q v = v + cross (xyz__ q) (cross (xyz__ q) v + v * www__ q) * cnst (Vec3 2 2 2)

applyQuatOffset :: (OfScalarType a, Quaternionized a) => Node (Vec4 a) -> Node (Vec3 a) -> Node (Vec3 a) -> Node (Vec3 a)
applyQuatOffset q o v = applyQuat q v + o
