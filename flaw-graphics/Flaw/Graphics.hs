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
	, Render
	, renderScope
	, renderFrameBuffer
	, renderViewport
	, renderGetViewport
	, renderVertexBuffer
	, renderIndexBuffer
	, renderUniformBuffer
	, renderSampler
	, renderProgram
	, renderClearColor
	, renderClearDepth
	, renderClearStencil
	, renderClearDepthStencil
	, renderDraw
	, renderPlay
	, render
	, present
	) where

import Flaw.Graphics.Internal
