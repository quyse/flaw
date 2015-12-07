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
	, DepthTestFunc(..)
	, Render
	, renderScope
	, renderFrameBuffer
	, renderViewport
	, renderGetViewport
	, renderVertexBuffer
	, renderIndexBuffer
	, renderUniformBuffer
	, renderSampler
	, renderBlendState
	, renderDepthTestFunc
	, renderDepthWrite
	, renderProgram
	, renderClearColor
	, renderClearDepth
	, renderClearStencil
	, renderClearDepthStencil
	, renderUploadUniformBuffer
	, renderUploadVertexBuffer
	, renderDraw
	, renderDrawInstanced
	, renderPlay
	, render
	, present
	, renderRelativeViewport
	) where

import Flaw.Graphics.Internal
import Flaw.Math

-- | Helper function to set up viewport relatively to currently set viewport.
renderRelativeViewport :: Context c d => Vec4 Int -> Render c ()
renderRelativeViewport (Vec4 left top right bottom) = do
	Vec4 currentLeft currentTop _ _ <- renderGetViewport
	renderViewport $ Vec4 (left + currentLeft) (top + currentTop) (right + currentLeft) (bottom + currentTop)
