{-|
Module: Flaw.Js
Description: General javascript things.
License: MIT
-}

{-# LANGUAGE FlexibleInstances, JavaScriptFFI, TypeSynonymInstances #-}

module Flaw.Js
	( initJs
	, byteStringToJsDataView
	, byteStringToJsUint8Array
	, byteStringToJsUint16Array
	, byteStringToJsUint32Array
	, byteStringToJsFloatArray
	, arrayBufferToByteString
	, blobToArrayBuffer
	, ptrToFloat32Array
	, ptrToInt32Array
	, ptrToUint32Array
	, arrayBufferFromUrl
	, HasObjectUrl(..)
	, revokeObjectUrl
	) where

import qualified Data.ByteString as B
import Foreign.Ptr
import qualified GHCJS.Buffer
import GHCJS.Types
import JavaScript.TypedArray.ArrayBuffer(ArrayBuffer)
import JavaScript.Web.Blob(Blob)

-- Even if nothing has to be done, the module has to contain some code called by some other
-- code (currently flaw-app calls it), otherwise it's excluded from linking together with javascript sources.

foreign import javascript interruptible "h$flaw_js_init($c);" initJs :: IO ()

-- | Convert bytestring to Javascript DataView object.
-- Looks like there's no official GHCJS (exported) function to get a view for a part of the buffer.
-- There's only non-exported JavaScript.TypedArray.DataView.dataView' function.
byteStringToJsDataView :: B.ByteString -> JSVal
byteStringToJsDataView bytes = js_dataViewFromBuffer buf off len where
	(buf, off, len) = GHCJS.Buffer.fromByteString bytes

-- | Convert bytestring to Javascript Uint8Array object.
byteStringToJsUint8Array :: B.ByteString -> JSVal
byteStringToJsUint8Array bytes = js_uint8ArrayFromBuffer buf off len where
	(buf, off, len) = GHCJS.Buffer.fromByteString bytes

-- | Convert bytestring to Javascript Uint16Array object.
byteStringToJsUint16Array :: B.ByteString -> JSVal
byteStringToJsUint16Array bytes = js_uint16ArrayFromBuffer buf off len where
	(buf, off, len) = GHCJS.Buffer.fromByteString bytes

-- | Convert bytestring to Javascript Uint32Array object.
byteStringToJsUint32Array :: B.ByteString -> JSVal
byteStringToJsUint32Array bytes = js_uint32ArrayFromBuffer buf off len where
	(buf, off, len) = GHCJS.Buffer.fromByteString bytes

-- | Convert bytestring to Javascript FloatArray object.
byteStringToJsFloatArray :: B.ByteString -> JSVal
byteStringToJsFloatArray bytes = js_floatArrayFromBuffer buf off len where
	(buf, off, len) = GHCJS.Buffer.fromByteString bytes

foreign import javascript unsafe "new DataView($1.buf, $2, $3)" js_dataViewFromBuffer :: GHCJS.Buffer.Buffer -> Int -> Int -> JSVal
foreign import javascript unsafe "new Uint8Array($1.buf, $2, $3)" js_uint8ArrayFromBuffer :: GHCJS.Buffer.Buffer -> Int -> Int -> JSVal
foreign import javascript unsafe "new Uint16Array($1.buf, $2, $3)" js_uint16ArrayFromBuffer :: GHCJS.Buffer.Buffer -> Int -> Int -> JSVal
foreign import javascript unsafe "new Uint32Array($1.buf, $2, $3)" js_uint32ArrayFromBuffer :: GHCJS.Buffer.Buffer -> Int -> Int -> JSVal
foreign import javascript unsafe "new FloatArray($1.buf, $2, $3)" js_floatArrayFromBuffer :: GHCJS.Buffer.Buffer -> Int -> Int -> JSVal

arrayBufferToByteString :: ArrayBuffer -> B.ByteString
arrayBufferToByteString = GHCJS.Buffer.toByteString 0 Nothing . GHCJS.Buffer.createFromArrayBuffer

-- | Convert Blob to ArrayBuffer.
foreign import javascript interruptible "h$flaw_js_blob_to_array_buffer($1, $c);" blobToArrayBuffer :: Blob -> ArrayBuffer

-- Converting pointers to arrays.

foreign import javascript unsafe "$1.f3.subarray($1_2 >> 2, ($1_2 >> 2) + $2)" ptrToFloat32Array :: Ptr Float -> Int -> JSVal
foreign import javascript unsafe "$1.i3.subarray($1_2 >> 2, ($1_2 >> 2) + $2)" ptrToInt32Array :: Ptr Int -> Int -> JSVal
foreign import javascript unsafe "$1.u3.subarray($1_2 >> 2, ($1_2 >> 2) + $2)" ptrToUint32Array :: Ptr Word -> Int -> JSVal

-- | Load binary data by URL.
foreign import javascript interruptible "h$flaw_js_load_url($1, $c);" arrayBufferFromUrl :: JSString -> IO ArrayBuffer

-- | Class of things which could be converted to URL.
class HasObjectUrl a where
	-- | Create URL representing object, with specified MIME type.
	createObjectUrl :: JSString -> a -> IO JSString

-- | Revoke URL representing object.
foreign import javascript unsafe "URL.revokeObjectURL($1)" revokeObjectUrl :: JSString -> IO ()

instance HasObjectUrl JSVal where
	createObjectUrl = objectUrl

instance HasObjectUrl B.ByteString where
	createObjectUrl mime = objectUrl mime . byteStringToJsDataView

instance HasObjectUrl ArrayBuffer where
	createObjectUrl mime = objectUrl mime . jsval

foreign import javascript unsafe "h$flaw_js_object_url($1, $2)" objectUrl :: JSString -> JSVal -> IO JSString
