{-|
Module: Flaw.Window.Web.Canvas
Description: Web canvas as a window.
License: MIT
-}

{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Flaw.Window.Web.Canvas
  ( WebWindowSystem(..)
  , Canvas(..)
  , runWebWindowSystem
  , initCanvas
  , setCanvasFullscreen
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Text as T
import GHCJS.Marshal.Pure
import GHCJS.Types

import Flaw.Window

data WebWindowSystem = WebWindowSystem

data Canvas = Canvas
  { canvasElement :: {-# UNPACK #-} !JSVal
  , canvasEventsChan :: {-# UNPACK #-} !(TChan WindowEvent)
  }

runWebWindowSystem :: MVar (WebWindowSystem, IO ()) -> IO ()
runWebWindowSystem resultVar = do
  stopVar <- newEmptyMVar
  putMVar resultVar (WebWindowSystem, putMVar stopVar ())
  takeMVar stopVar

initCanvas :: WebWindowSystem -> T.Text -> IO Canvas
initCanvas WebWindowSystem title = do
  jsCanvas <- js_initCanvas
  eventsChan <- newBroadcastTChanIO
  let canvas = Canvas
    { canvasElement = jsCanvas
    , canvasEventsChan = eventsChan
    }
  setWindowTitle canvas title
  return canvas

instance Window Canvas where
  setWindowTitle _canvas title = js_setTitle $ pToJSVal title
  {-# INLINABLE getWindowClientSize #-}
  getWindowClientSize Canvas
    { canvasElement = jsCanvas
    } = do
    width <- js_clientWidth jsCanvas
    height <- js_clientHeight jsCanvas
    return (width, height)
  chanWindowEvents Canvas
    { canvasEventsChan = eventsChan
    } = dupTChan eventsChan

  setWindowMouseCursor Canvas
    { canvasElement = jsCanvas
    } cursor = js_setCursor jsCanvas $ case cursor of
    MouseCursorArrow -> "arrow"
    MouseCursorWait -> "wait"
    MouseCursorWaitArrow -> "progress"
    MouseCursorIBeam -> "text"
    MouseCursorSizeNWSE -> "nwse-resize"
    MouseCursorSizeNESW -> "nesw-resize"
    MouseCursorSizeWE -> "ew-resize"
    MouseCursorSizeNS -> "ns-resize"
    MouseCursorSizeAll -> "grab"
    MouseCursorHand -> "pointer"

  setWindowMouseLock Canvas
    { canvasElement = jsCanvas
    } = js_setMouseLock jsCanvas

setCanvasFullscreen :: Canvas -> Bool -> IO ()
setCanvasFullscreen Canvas
  { canvasElement = jsCanvas
  } = js_setFullscreen jsCanvas

foreign import javascript unsafe "h$flaw_window_init_canvas" js_initCanvas :: IO JSVal

foreign import javascript unsafe "document.title=$1" js_setTitle :: JSVal -> IO ()

foreign import javascript unsafe "$1.clientWidth" js_clientWidth :: JSVal -> IO Int
foreign import javascript unsafe "$1.clientHeight" js_clientHeight :: JSVal -> IO Int

foreign import javascript unsafe "$1.style.cursor = $2" js_setCursor :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.flaw_window_set_mouse_lock($2)" js_setMouseLock :: JSVal -> Bool -> IO ()
foreign import javascript unsafe "$1.flaw_window_set_fullscreen($2)" js_setFullscreen :: JSVal -> Bool -> IO ()
