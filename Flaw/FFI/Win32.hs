{-|
Module: Flaw.FFI.Win32
Description: Some types from Win32.
License: MIT
-}

{-# LANGUAGE TemplateHaskell #-}

module Flaw.FFI.Win32
	( BOOL
	, BYTE
	, CHAR
	, WCHAR
	, INT
	, UINT
	, LONG
	, ULONG
	, WORD
	, DWORD
	, SIZE_T
	, LARGE_INTEGER
	, RECT(..)
	, GUID
	, HANDLE
	, HINSTANCE
	, HMODULE
	, HWND
	, loadLibrary
	, getProcAddress
	, winUTF16toText
	) where

import Control.Monad
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.UUID
import Data.Word
import Foreign.C.String
import Foreign.Ptr

import Flaw.FFI

type BOOL = Bool
type BYTE = Word8
type CHAR = Word8
type WCHAR = Word16
type INT = Int32
type UINT = Word32
type LONG = Int32
type ULONG = Word32
type WORD = Word16
type DWORD = Word32
type SIZE_T = Word
type LARGE_INTEGER = Int64

genStruct "RECT"
	[ ([t|LONG|], "left", 0)
	, ([t|LONG|], "top", 0)
	, ([t|LONG|], "right", 0)
	, ([t|LONG|], "bottom", 0)
	]

type GUID = UUID

type HANDLE = Ptr ()

type HINSTANCE = HANDLE
type HMODULE = HINSTANCE

type HWND = HANDLE

loadLibrary :: String -> IO HMODULE
loadLibrary name = withCWString name winapi_loadLibraryW

foreign import stdcall safe "LoadLibraryW" winapi_loadLibraryW :: CWString -> IO HMODULE

getProcAddress :: HMODULE -> String -> IO (Ptr a)
getProcAddress hmodule functionName = liftM castPtr $ withCString functionName $ winapi_getProcAddress hmodule

foreign import stdcall safe "GetProcAddress" winapi_getProcAddress :: HMODULE -> CString -> IO (Ptr ())

-- | Convert LPWSTR to Text.
winUTF16toText :: [WCHAR] -> T.Text
winUTF16toText s = T.decodeUtf16LE $ BL.toStrict $ BSB.toLazyByteString $ upToZero s where
	upToZero (x:xs) = if x == 0 then mempty else mappend (BSB.word16LE x) $ upToZero xs
	upToZero [] = mempty
