{-|
Module: Flaw.Graphics.DirectX11.Internal
Description: Internals of graphics implementation for DirectX 11.
License: MIT
-}

{-# LANGUAGE TypeFamilies #-}

module Flaw.Graphics.DirectX11.Internal
	( Dx11Device(..)
	, Dx11DeviceContext(..)
	) where

import qualified Control.Exception.Lifted as Lifted
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Ratio
import qualified Data.Text as T
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Flaw.Exception
import Flaw.FFI
import Flaw.FFI.COM
import Flaw.FFI.Win32
import Flaw.Graphics.DirectX11.FFI
import Flaw.Graphics.DXGI
import Flaw.Graphics.Internal

-- | DirectX11 graphics device.
data Dx11Device = Dx11Device
	{ dx11DeviceInterface :: ID3D11Device
	, dx11DeviceImmediateContext :: Dx11DeviceContext
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
					hresultCheck =<< m_IDXGIOutput_GetDisplayModeList output (wrapEnum DXGI_FORMAT_R8G8B8A8_UNORM) 0 modesCountPtr nullPtr
					modesCount <- liftM fromIntegral $ peek modesCountPtr
					allocaArray modesCount $ \modeDescsPtr -> do
						hresultCheck =<< m_IDXGIOutput_GetDisplayModeList output (wrapEnum DXGI_FORMAT_R8G8B8A8_UNORM) 0 modesCountPtr modeDescsPtr
						peekArray modesCount modeDescsPtr
				-- make id-info output pairs
				let modes = [(Dx11DisplayModeId modeDesc, displayModeInfoFromDesc modeDesc) | modeDesc <- modeDescs]
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

	createDevice (Dx11DeviceId adapter) = describeException "failed to create DirectX11 graphics device" $ do
		let create = alloca $ \devicePtr -> alloca $ \deviceContextPtr -> do
			let featureLevels = [D3D_FEATURE_LEVEL_11_1, D3D_FEATURE_LEVEL_11_0, D3D_FEATURE_LEVEL_10_1, D3D_FEATURE_LEVEL_10_0]
			withArray featureLevels $ \featureLevelsPtr -> alloca $ \realFeatureLevelPtr -> do
				hresultCheck =<< d3d11CreateDevice (pokeCOMObject adapter) (wrapEnum D3D_DRIVER_TYPE_HARDWARE) nullPtr 0 featureLevelsPtr (fromIntegral $ length featureLevels) d3d11SdkVersion devicePtr realFeatureLevelPtr deviceContextPtr
			device <- peekCOMObject =<< peek devicePtr
			deviceContext <- peekCOMObject =<< peek deviceContextPtr
			return (device, deviceContext)
		let destroy (device, deviceContext) = do
			_ <- m_IUnknown_Release device
			_ <- m_IUnknown_Release deviceContext
			return ()
		(releaseKey, (device, deviceContext)) <- allocate create destroy
		return (releaseKey, Dx11Device
			{ dx11DeviceInterface = device
			, dx11DeviceImmediateContext = Dx11DeviceContext
				{ dx11DeviceContextInterface = deviceContext
				}
			})

	createDisplayMode (Dx11DisplayId output) width height = describeException "failed to try create DirectX11 display mode" $ do
		let create = do
			let desc = DXGI_MODE_DESC
				{ f_DXGI_MODE_DESC_Width = fromIntegral width
				, f_DXGI_MODE_DESC_Height = fromIntegral height
				, f_DXGI_MODE_DESC_RefreshRate = DXGI_RATIONAL
					{ f_DXGI_RATIONAL_Numerator = 0
					, f_DXGI_RATIONAL_Denominator = 0
					}
				, f_DXGI_MODE_DESC_Format = DXGI_FORMAT_R8G8B8A8_UNORM
				, f_DXGI_MODE_DESC_ScanlineOrdering = DXGI_MODE_SCANLINE_ORDER_UNSPECIFIED
				, f_DXGI_MODE_DESC_Scaling = DXGI_MODE_SCALING_UNSPECIFIED
				}
			closestDesc <- with desc $ \descPtr -> createCOMValueViaPtr $ \closestDescPtr -> m_IDXGIOutput_FindClosestMatchingMode output descPtr closestDescPtr nullPtr
			return (Dx11DisplayModeId closestDesc, displayModeInfoFromDesc closestDesc)
		let destroy (_id, _info) = return ()
		allocate create destroy

-- | DirectX11 graphics context.
data Dx11DeviceContext = Dx11DeviceContext
	{ dx11DeviceContextInterface :: ID3D11DeviceContext
	}

-- | Convert DXGI_MODE_DESC to DisplayModeInfo.
displayModeInfoFromDesc :: DXGI_MODE_DESC -> DisplayModeInfo
displayModeInfoFromDesc desc = info where
	info = DisplayModeInfo
		{ displayModeName = T.pack $ show desc
		, displayModeWidth = fromIntegral $ f_DXGI_MODE_DESC_Width desc
		, displayModeHeight = fromIntegral $ f_DXGI_MODE_DESC_Height desc
		, displayModeRefreshRate = (fromIntegral $ f_DXGI_RATIONAL_Numerator refreshRate) % (fromIntegral $ f_DXGI_RATIONAL_Denominator refreshRate)
		}
	refreshRate = f_DXGI_MODE_DESC_RefreshRate desc
