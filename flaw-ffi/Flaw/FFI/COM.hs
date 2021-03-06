{-|
Module: Flaw.FFI.COM
Description: Integration with Windows COM.
License: MIT
-}

{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, TemplateHaskell #-}

module Flaw.FFI.COM
  ( HRESULT
  , IID
  , REFGUID
  , REFIID
  , COMInterface(getIID, pokeCOMObject)
  , peekCOMObject
  , hresultSucceeded
  , hresultFailed
  , FailedHRESULT(..)
  , hresultCheck
  , IUnknown(..)
  , IUnknown_Class(..)
  , comInitialize
  , comUninitialize
  , comQueryInterface
  , withCOMObject
  , createCOMValueViaPtr
  , createCOMObjectViaPtr
  , allocateCOMObject
  ) where

import Control.Exception
import Control.Monad
import Data.UUID
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import Flaw.FFI.COM.Internal
import Flaw.FFI.COM.TH
import Flaw.FFI.Win32

genCOMInterface "IUnknown" "00000000-0000-0000-C000-000000000046" []
  [ ([t| Ptr UUID -> Ptr (Ptr ()) -> IO HRESULT |], "QueryInterface")
  , ([t| IO ULONG |], "AddRef")
  , ([t| IO ULONG |], "Release")
  ]

-- | Initialization of COM for the current thread.
comInitialize :: IO ()
comInitialize = do
  hr <- winapi_CoInitialize nullPtr
  when (hresultFailed hr) $ fail "cannot initialize COM"

foreign import stdcall safe "CoInitialize" winapi_CoInitialize :: Ptr () -> IO HRESULT

-- | Uninitialize COM for current thread.
comUninitialize :: IO ()
comUninitialize = winapi_CoUninitialize

foreign import stdcall safe "CoUninitialize" winapi_CoUninitialize :: IO ()

-- | Strictly typed QueryInterface.
comQueryInterface :: forall a b. (IUnknown_Class a, COMInterface b) => a -> IO (Maybe b)
comQueryInterface this = alloca $ \out -> do
  hr <- alloca $ \iid -> do
    poke iid $ getIID (undefined :: b)
    m_IUnknown_QueryInterface this iid out
  if hresultFailed hr then return Nothing
  else fmap Just $ peekCOMObject . castPtr =<< peek out

-- | Ensure release of COM object after computation.
withCOMObject :: IUnknown_Class a => IO a -> (a -> IO b) -> IO b
withCOMObject create = bracket create m_IUnknown_Release

-- | Get value from computation working with pointer.
createCOMValueViaPtr :: Storable a => (Ptr a -> IO HRESULT) -> IO a
createCOMValueViaPtr create = alloca $ \p -> do
  hresultCheck =<< create p
  peek p

-- | Get COM object from computation working with pointer.
createCOMObjectViaPtr :: COMInterface a => (Ptr (Ptr a) -> IO HRESULT) -> IO a
createCOMObjectViaPtr create = peekCOMObject =<< createCOMValueViaPtr create

-- | Allocate COM object in ResourceT.
allocateCOMObject :: IUnknown_Class a => IO a -> IO (a, IO ())
allocateCOMObject create = do
  object <- create
  return (object, void $ m_IUnknown_Release object)
