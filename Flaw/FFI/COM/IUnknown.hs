{-|
Module: Flaw.FFI.COM.IUnknown
Description: IUnknown COM interface.
License: MIT
-}

{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, RankNTypes #-}

module Flaw.FFI.COM.IUnknown
	( IUnknown(..)
	, IUnknown_Class(..)
	, comQueryInterface
	) where

import Control.Monad
import Data.UUID
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import Flaw.FFI.COM

genCOMInterface "IUnknown" "00000000-0000-0000-C000-000000000046" Nothing
	[ ([t| Ptr UUID -> Ptr (Ptr ()) -> IO HRESULT |], "QueryInterface")
	, ([t| IO Word |], "AddRef")
	, ([t| IO Word |], "Release")
	]

-- | Strictly typed QueryInterface.
comQueryInterface :: forall a b. (IUnknown_Class a, COMInterface b) => a -> IO (Maybe b)
comQueryInterface this = alloca $ \out -> do
	hr <- alloca $ \iid -> do
		poke iid $ getIID (undefined :: b)
		m_IUnknown_QueryInterface this iid (out :: Ptr (Ptr ()))
	if hresultFailed hr then return Nothing
	else liftM Just $ getCOMObject =<< peek out
