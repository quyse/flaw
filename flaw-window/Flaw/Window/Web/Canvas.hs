{-|
Module: Flaw.Window.Web.Canvas
Description: Web canvas as a window.
License: MIT
-}

{-# LANGUAGE JavaScriptFFI #-}

module Flaw.Window.Web.Canvas
	( Canvas(..)
	, initCanvas
	) where

import Control.Monad
import Data.Maybe
import GHCJS.Marshal
import GHCJS.Types
import qualified GHCJS.DOM.Element as DOM

import Flaw.Window

data Canvas = Canvas DOM.Element

initCanvas :: Int -> Int -> IO Canvas
initCanvas width height = do
	jsCanvas <- js_initCanvas width height
	maybeDomCanvas <- fromJSRef jsCanvas
	return $ Canvas $ fromJust maybeDomCanvas

instance Window Canvas where
	setWindowTitle _ _ = return ()
	getWindowClientSize (Canvas domCanvas) = do
		width <- liftM floor $ DOM.elementGetClientWidth domCanvas
		height <- liftM floor $ DOM.elementGetClientHeight domCanvas
		return (width, height)
	addWindowCallback _ _ = return ()

foreign import javascript unsafe " \
	\ var c = document.createElement('canvas'); \
	\ c.width = $1; \
	\ c.height = $2; \
	\ document.body.appendChild(c); \
	\ $r=c" js_initCanvas :: Int -> Int -> IO (JSRef DOM.Element)
