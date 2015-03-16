{-|
Module: Flaw.Graphics.DXGI
Description: DXGI integration.
License: MIT
-}

{-# LANGUAGE TemplateHaskell #-}

module Flaw.Graphics.DXGI
	( DXGISystem
	, dxgiCreateSystem
	, DeviceId
	, DisplayId
	, DisplayModeId
	) where

import Flaw.Graphics.DXGI.Internal
