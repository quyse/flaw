{-|
Module: Flaw.Graphics
Description: Main file with types for graphics.
License: MIT
-}

module Flaw.Graphics
	( System(..)
	, Device(..)
	, Context(..)
	, Presenter(..)
	, DeviceInfo(..)
	, DisplayInfo(..)
	, DisplayModeInfo(..)
	, Render(..)
	, renderScope
	, renderFrameBuffer
	, renderVertexBuffers
	, renderIndexBuffer
	, renderProgram
	, renderClearColor
	, renderClearDepth
	, renderClearStencil
	, renderClearDepthStencil
	, renderDraw
	, renderPlay
	) where

import Flaw.Graphics.Internal
