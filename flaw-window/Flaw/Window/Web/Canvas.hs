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
import Data.Maybe
import qualified Data.Text as T
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Types
import qualified GHCJS.DOM.Element as DOM

import Flaw.Window

data Canvas = Canvas
	{ canvasElement :: !DOM.Element
	, canvasEventsChan :: !(TChan WindowEvent)
	}

initCanvas :: T.Text -> IO Canvas
initCanvas title = do
	jsCanvas <- js_initCanvas
	eventsChan <- newBroadcastTChanIO
	let canvas = Canvas
		{ canvasElement = pFromJSVal jsCanvas
		, canvasEventsChan = eventsChan
		}
	setWindowTitle canvas title
	return canvas

instance Window Canvas where
	setWindowTitle _canvas title = js_setTitle $ pToJSVal title
	getWindowClientSize Canvas
		{ canvasElement = domCanvas
		} = do
		width <- fmap floor $ DOM.getClientWidth domCanvas
		height <- fmap floor $ DOM.getClientHeight domCanvas
		return (width, height)
	chanWindowEvents Canvas
		{ canvasEventsChan = eventsChan
		} = dupTChan eventsChan

foreign import javascript unsafe "h$flaw_window_init_canvas" js_initCanvas :: IO JSVal

foreign import javascript unsafe "document.title=$1" js_setTitle :: JSVal -> IO ()
