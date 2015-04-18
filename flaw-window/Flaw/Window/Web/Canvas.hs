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
import qualified Data.Text as T
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types
import qualified GHCJS.DOM.Element as DOM

import Flaw.Window

data Canvas = Canvas DOM.Element

initCanvas :: T.Text -> IO Canvas
initCanvas title = do
	jsCanvas <- js_initCanvas
	maybeDomCanvas <- fromJSRef jsCanvas
	let canvas = Canvas $ fromJust maybeDomCanvas
	setWindowTitle canvas title
	return canvas

instance Window Canvas where
	setWindowTitle _canvas title = js_setTitle $ toJSString title
	getWindowClientSize (Canvas domCanvas) = do
		width <- liftM floor $ DOM.elementGetClientWidth domCanvas
		height <- liftM floor $ DOM.elementGetClientHeight domCanvas
		return (width, height)
	addWindowCallback _ _ = return ()

foreign import javascript unsafe " \
	\ var c = document.createElement('canvas'); \
	\ c.style.position = 'absolute'; \
	\ c.style.left = 0; \
	\ c.style.top = 0; \
	\ c.width = window.innerWidth; \
	\ c.height = window.innerHeight; \
	\ document.body.appendChild(c); \
	\ document.body.style.overflow = 'hidden'; \
	\ window.addEventListener('resize', function() { \
	\ c.width = window.innerWidth; \
	\ c.height = window.innerHeight; \
	\ }, false); \
	\ $r=c" js_initCanvas :: IO (JSRef DOM.Element)

foreign import javascript unsafe "document.title=$1" js_setTitle :: JSString -> IO ()
