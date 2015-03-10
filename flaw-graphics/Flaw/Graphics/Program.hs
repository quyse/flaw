{-|
Module: Flaw.Graphics.Program
Description: Shader program support.
License: MIT
-}

module Flaw.Graphics.Program
	( Program
	, AttributeFormat(..)
	, UniformFormat(..)
	, cnst
	, attribute
	, uniform
	, sampler
	, temp
	, rasterize
	, colorTarget
	, colorDepthTarget
	) where

import Flaw.Graphics.Program.Internal
