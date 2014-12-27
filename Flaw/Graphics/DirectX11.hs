{-|
Module: Flaw.Graphics.DirectX11
Description: Graphics implementation for DirectX 11.
License: MIT
-}

{-# LANGUAGE TypeFamilies #-}

module Flaw.Graphics.DirectX11
	( Dx11Device
	, Dx11DeviceContext
	) where

import qualified Control.Exception.Lifted as Lifted
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Ratio
import qualified Data.Text as T
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Flaw.FFI
import Flaw.FFI.COM
import Flaw.FFI.Win32
import Flaw.Graphics.DirectX11.FFI
import Flaw.Graphics.DXGI
import Flaw.Graphics.Internal

data Dx11Device = Dx11Device
	{ dx11DeviceInterface :: ID3D11Device
	}

instance Device Dx11Device where
	newtype DeviceId Dx11Device = Dx11DeviceId IDXGIAdapter
	newtype DisplayId Dx11Device = Dx11DisplayId IDXGIOutput
	newtype DisplayModeId Dx11Device = Dx11DisplayModeId DXGI_MODE_DESC
	type DeviceContext Dx11Device = Dx11DeviceContext

	getDevices = do
		-- create DXGI factory
		(dxgiFactoryReleaseKey, dxgiFactory) <- allocateCOMObject createDXGIFactory
		-- enumerate adapters with release keys
		let enumerateAdapter i = Lifted.handle (\(FailedHRESULT _hr) -> return []) $ do
			releaseKeyAndAdapter <- allocateCOMObject $ createCOMObjectViaPtr $ m_IDXGIFactory_EnumAdapters dxgiFactory i
			rest <- enumerateAdapter $ i + 1
			return $ releaseKeyAndAdapter : rest
		-- make id-info adapter pairs
		let makeAdapterIdInfo (adapterReleaseKey, adapter) = do
			-- get adapter desc
			adapterDesc <- liftIO $ createCOMValueViaPtr $ m_IDXGIAdapter_GetDesc adapter
			-- enumerate outputs
			let enumerateOutput i = Lifted.handle (\(FailedHRESULT _hr) -> return []) $ do
				releaseKeyAndOutput <- allocateCOMObject $ createCOMObjectViaPtr $ m_IDXGIAdapter_EnumOutputs adapter i
				rest <- enumerateOutput $ i + 1
				return $ releaseKeyAndOutput : rest
			-- make id-info output pairs
			let makeOutputIdInfo (outputReleaseKey, output) = do
				-- get output desc
				outputDesc <- liftIO $ createCOMValueViaPtr $ m_IDXGIOutput_GetDesc output
				-- enumerate modes
				modeDescs <- liftIO $ alloca $ \modesCountPtr -> do
					hr <- m_IDXGIOutput_GetDisplayModeList output (wrapEnum DXGI_FORMAT_R8G8B8A8_UNORM) 0 modesCountPtr nullPtr
					hresultCheck hr
					modesCount <- liftM fromIntegral $ peek modesCountPtr
					allocaArray modesCount $ \modeDescsPtr -> do
						hr <- m_IDXGIOutput_GetDisplayModeList output (wrapEnum DXGI_FORMAT_R8G8B8A8_UNORM) 0 modesCountPtr modeDescsPtr
						hresultCheck hr
						peekArray modesCount modeDescsPtr
				-- make id-info output pairs
				let makeModeIdInfo modeDesc = (Dx11DisplayModeId modeDesc, modeInfo) where
					modeInfo = DisplayModeInfo
						{ displayModeName = T.pack $ show modeDesc
						, displayModeWidth = fromIntegral $ f_DXGI_MODE_DESC_Width modeDesc
						, displayModeHeight = fromIntegral $ f_DXGI_MODE_DESC_Height modeDesc
						, displayModeRefreshRate = (fromIntegral $ f_DXGI_RATIONAL_Numerator refreshRate) % (fromIntegral $ f_DXGI_RATIONAL_Denominator refreshRate)
						}
					refreshRate = f_DXGI_MODE_DESC_RefreshRate modeDesc
				let modes = map makeModeIdInfo modeDescs
				-- return output pair
				return (outputReleaseKey, (Dx11DisplayId output, DisplayInfo
					{ displayName = winUTF16ToText $ f_DXGI_OUTPUT_DESC_DeviceName outputDesc
					, displayModes = modes
					}))
			outputs <- mapM makeOutputIdInfo =<< enumerateOutput 0
			-- create adapter compound release key
			adapterCompoundReleaseKey <- register $ do
				release adapterReleaseKey
				mapM_ (release . fst) outputs
			-- return adapter pair
			return (adapterCompoundReleaseKey, (Dx11DeviceId adapter, DeviceInfo
				{ deviceName = winUTF16ToText $ f_DXGI_ADAPTER_DESC_Description adapterDesc
				, deviceDisplays = map snd outputs
				}))
		adapters <- mapM makeAdapterIdInfo =<< enumerateAdapter 0
		release dxgiFactoryReleaseKey
		-- create compound release key
		compoundReleaseKey <- register $ mapM_ (release . fst) adapters
		-- return adapters
		return (compoundReleaseKey, map snd adapters)

data Dx11DeviceContext = Dx11DeviceContext
	{ dx11DeviceContextInterface :: ID3D11DeviceContext
	}
