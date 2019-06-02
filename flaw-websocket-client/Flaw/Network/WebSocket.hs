{-|
Module: Flaw.Network.WebSocket
Description: WebSocket client.
License: MIT
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Flaw.Network.WebSocket
  ( WebSocket(..)
  , newSocket
  ) where

import Control.Concurrent.STM
import Control.Monad
import Data.JSString.Text
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHCJS.Foreign.Callback
import GHCJS.Types
import qualified JavaScript.Web.WebSocket as WS
import qualified JavaScript.Web.MessageEvent as WME
import Unsafe.Coerce

import Flaw.Book
import Flaw.Flow
import Flaw.Js
import Flaw.Network
import Flaw.Network.BiuoSocket

newtype WebSocket = WebSocket BiuoSocket deriving Socket

newSocket :: T.Text -> IO (WebSocket, IO ())
newSocket url = withSpecialBook $ \bk -> do
  websocket <- WS.connect WS.WebSocketRequest
    { WS.url = textToJSString url
    , WS.protocols = []
    , WS.onClose = Nothing
    , WS.onMessage = Nothing
    }
  book bk $ return ((), WS.close Nothing Nothing websocket)

  socket@BiuoSocket
    { biuoSocketInQueue = inQueue
    , biuoSocketOutQueue = outQueue
    } <- atomically $ newBiuoSocket 1

  callback <- asyncCallback1 $ \messageEvent -> do
    let bytes = case WME.getData $ unsafeCoerce messageEvent of
      WME.StringData s -> T.encodeUtf8 $ textFromJSString s
      WME.BlobData b -> arrayBufferToByteString $ blobToArrayBuffer b
      WME.ArrayBufferData d -> arrayBufferToByteString d
    atomically $ writeTBQueue inQueue bytes
  book bk $ return ((), releaseCallback callback)

  book bk $ forkFlow $ forever $ js_send websocket . byteStringToJsDataView =<< atomically (readTQueue outQueue)

  js_setOnMessage websocket callback

  return $ WebSocket socket

foreign import javascript unsafe "$1.onmessage = $2" js_setOnMessage :: WS.WebSocket -> Callback (JSVal -> IO ()) -> IO ()
foreign import javascript unsafe "$1.send($2)" js_send :: WS.WebSocket -> JSVal -> IO ()
