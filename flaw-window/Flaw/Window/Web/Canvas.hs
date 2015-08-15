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

import Control.Concurrent.STM
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Types
import qualified GHCJS.DOM.Element as DOM

import Flaw.Window

data Canvas = Canvas
	{ canvasElement :: DOM.Element
	, canvasEventsChan :: TChan WindowEvent
	}

initCanvas :: T.Text -> IO Canvas
initCanvas title = do
	jsCanvas <- js_initCanvas
	maybeDomCanvas <- fromJSRef jsCanvas
	eventsChan <- newBroadcastTChanIO
	let canvas = Canvas
		{ canvasElement = fromJust maybeDomCanvas
		, canvasEventsChan = eventsChan
		}
	setWindowTitle canvas title
	return canvas

instance Window Canvas where
	setWindowTitle _canvas title = js_setTitle $ pToJSRef title
	getWindowClientSize Canvas
		{ canvasElement = domCanvas
		} = do
		width <- liftM floor $ DOM.getClientWidth domCanvas
		height <- liftM floor $ DOM.getClientHeight domCanvas
		return (width, height)
	chanWindowEvents Canvas
		{ canvasEventsChan = eventsChan
		} = dupTChan eventsChan

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

foreign import javascript unsafe "document.title=$1" js_setTitle :: JSRef T.Text -> IO ()
