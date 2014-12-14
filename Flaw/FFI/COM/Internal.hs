{-|
Module: Flaw.FFI.COM.Internal
Description: Internal things for integration with Windows COM.
License: MIT
-}

module Flaw.FFI.COM.Internal
	( HRESULT
	, IID
	, REFGUID
	, REFIID
	, hresultSucceeded
	, hresultFailed
	, COMInterface(..)
	, peekCOMObject
	) where

import Foreign.Ptr
import Foreign.Storable

import Flaw.FFI.Win32

type HRESULT = LONG -- should be signed!
type IID = GUID
type REFGUID = Ptr GUID
type REFIID = Ptr IID

-- | Class of COM interface.
class COMInterface i where
	-- | Get IID of COM interface. Argument is not used.
	getIID :: i -> IID
	-- | Get size of virtual table. Argument is not used.
	sizeOfCOMVirtualTable :: i -> Int
	-- | Get native pointer to object.
	pokeCOMObject :: i -> Ptr i
	-- | Internal method to parse table of virtual methods.
	peekCOMVirtualTable
		:: Ptr i -- ^ 'this' pointer
		-> Ptr () -- ^ pointer to table of virtual methods
		-> IO i

-- | Get COM object from pointer.
peekCOMObject :: COMInterface a => Ptr a -> IO a
peekCOMObject this = peek ((castPtr this) :: Ptr (Ptr ())) >>= peekCOMVirtualTable this

-- | If HRESULT value represents success.
hresultSucceeded :: HRESULT -> Bool
hresultSucceeded hr = hr >= 0

-- | If HRESULT value represents failure.
hresultFailed :: HRESULT -> Bool
hresultFailed hr = hr < 0
