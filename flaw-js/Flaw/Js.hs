{-|
Module: Flaw.Js
Description: General javascript things.
License: MIT
-}

{-# LANGUAGE JavaScriptFFI #-}

module Flaw.Js
	( initJs
	, byteStringToJsDataView
	, arrayBufferToByteString
	, ptrToFloat32Array
	, ptrToInt32Array
	, ptrToUint32Array
	) where

import qualified Data.ByteString as B
import Foreign.Ptr
import qualified GHCJS.Buffer
import GHCJS.Types
import JavaScript.TypedArray.ArrayBuffer(ArrayBuffer)

-- Even if nothing has to be done, the module has to contain some code called by some other
-- code (currently flaw-app calls it), otherwise it's excluded from linking together with javascript sources.

foreign import javascript interruptible "h$flaw_js_init($c);" initJs :: IO ()

-- | Convert bytestring to Javascript DataView object.
-- Looks like there's no official GHCJS (exported) function to get a view for a part of the buffer.
-- There's only non-exported JavaScript.TypedArray.DataView.dataView' function.
byteStringToJsDataView :: B.ByteString -> JSVal
byteStringToJsDataView bytes = js_dataViewFromBuffer buf off len where
	(buf, off, len) = GHCJS.Buffer.fromByteString bytes

foreign import javascript unsafe "new DataView($1.buf, $2, $3)" js_dataViewFromBuffer :: GHCJS.Buffer.Buffer -> Int -> Int -> JSVal

arrayBufferToByteString :: ArrayBuffer -> B.ByteString
arrayBufferToByteString = GHCJS.Buffer.toByteString 0 Nothing . GHCJS.Buffer.createFromArrayBuffer

-- Converting pointers to arrays.

foreign import javascript unsafe "$1.f3.subarray($1_2, $1_2 + $2)" ptrToFloat32Array :: Ptr Float -> Int -> JSVal
foreign import javascript unsafe "$1.i3.subarray($1_2, $1_2 + $2)" ptrToInt32Array :: Ptr Int -> Int -> JSVal
foreign import javascript unsafe "$1.u3.subarray($1_2, $1_2 + $2)" ptrToUint32Array :: Ptr Word -> Int -> JSVal
