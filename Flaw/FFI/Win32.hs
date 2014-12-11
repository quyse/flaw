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
	) where

import Data.Int
import Data.UUID
import Data.Word
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
