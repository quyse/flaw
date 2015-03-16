{-|
Module: Flaw.Graphics.DXGI.Internal
Description: Internals of graphics implementation for DXGI.
License: MIT
-}

{-# LANGUAGE TypeFamilies #-}

module Flaw.Graphics.DXGI.Internal
	( DXGISystem
	, DeviceId(..)
	, DisplayId(..)
	, DisplayModeId(..)
	, getDXGIFormat
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
import Flaw.Graphics
import Flaw.Graphics.DXGI.FFI
import Flaw.Graphics.Texture

-- | DXGI graphics system.
data DXGISystem

instance System DXGISystem where
	newtype DeviceId DXGISystem = DXGIDeviceId IDXGIAdapter
	newtype DisplayId DXGISystem = DXGIDisplayId IDXGIOutput
	newtype DisplayModeId DXGISystem = DXGIDisplayModeId DXGI_MODE_DESC
	getInstalledDevices = describeException "failed to get installed DirectX11 devices" $ do
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
				let modes = [(DXGIDisplayModeId modeDesc, displayModeInfoFromDesc modeDesc) | modeDesc <- modeDescs]
				-- return output pair
				return (outputReleaseKey, (DXGIDisplayId output, DisplayInfo
					{ displayName = winUTF16ToText $ f_DXGI_OUTPUT_DESC_DeviceName outputDesc
					, displayModes = modes
					}))
			outputs <- mapM makeOutputIdInfo =<< enumerateOutput 0
			-- create adapter compound release key
			adapterCompoundReleaseKey <- register $ do
				release adapterReleaseKey
				mapM_ (release . fst) outputs
			-- return adapter pair
			return (adapterCompoundReleaseKey, (DXGIDeviceId adapter, DeviceInfo
				{ deviceName = winUTF16ToText $ f_DXGI_ADAPTER_DESC_Description adapterDesc
				, deviceDisplays = map snd outputs
				}))
		adapters <- mapM makeAdapterIdInfo =<< enumerateAdapter 0
		release dxgiFactoryReleaseKey
		-- create compound release key
		compoundReleaseKey <- register $ mapM_ (release . fst) adapters
		-- return adapters
		return (compoundReleaseKey, map snd adapters)
	createDisplayMode (DXGIDisplayId output) width height = describeException "failed to try create DirectX11 display mode" $ do
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
			return (DXGIDisplayModeId closestDesc, displayModeInfoFromDesc closestDesc)
		let destroy (_id, _info) = return ()
		allocate create destroy

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

-- | Convert TextureFormat to DXGI_FORMAT.
getDXGIFormat :: TextureFormat -> DXGI_FORMAT
getDXGIFormat t = case t of
	UncompressedTextureFormat
		{ textureFormatComponents = components
		, textureFormatValueType = valueType
		, textureFormatPixelSize = pixelSize
		, textureFormatColorSpace = colorSpace
		} -> case components of
			PixelR -> case valueType of
				PixelUntyped -> case pixelSize of
					Pixel8bit -> DXGI_FORMAT_R8_TYPELESS
					Pixel16bit -> DXGI_FORMAT_R16_TYPELESS
					Pixel32bit -> DXGI_FORMAT_R32_TYPELESS
					_ -> DXGI_FORMAT_UNKNOWN
				PixelUint -> case pixelSize of
					Pixel8bit -> DXGI_FORMAT_R8_UNORM
					Pixel16bit -> DXGI_FORMAT_R16_UNORM
					_ -> DXGI_FORMAT_UNKNOWN
				PixelFloat -> case pixelSize of
					Pixel16bit -> DXGI_FORMAT_R16_FLOAT
					Pixel32bit -> DXGI_FORMAT_R32_FLOAT
					_ -> DXGI_FORMAT_UNKNOWN
			PixelRG -> case valueType of
				PixelUntyped -> case pixelSize of
					Pixel16bit -> DXGI_FORMAT_R8G8_TYPELESS
					Pixel32bit -> DXGI_FORMAT_R16G16_TYPELESS
					_ -> DXGI_FORMAT_UNKNOWN
				PixelUint -> case pixelSize of
					Pixel16bit -> DXGI_FORMAT_R8G8_UNORM
					Pixel32bit -> DXGI_FORMAT_R16G16_UNORM
					_ -> DXGI_FORMAT_UNKNOWN
				PixelFloat -> case pixelSize of
					Pixel32bit -> DXGI_FORMAT_R16G16_FLOAT
					Pixel64bit -> DXGI_FORMAT_R32G32_FLOAT
					_ -> DXGI_FORMAT_UNKNOWN
			PixelRGB -> case valueType of
				PixelUntyped -> DXGI_FORMAT_UNKNOWN
				PixelUint -> DXGI_FORMAT_UNKNOWN
				PixelFloat -> case pixelSize of
					Pixel32bit -> DXGI_FORMAT_R11G11B10_FLOAT
					Pixel96bit -> DXGI_FORMAT_R32G32B32_FLOAT
					_ -> DXGI_FORMAT_UNKNOWN
			PixelRGBA -> case valueType of
				PixelUntyped -> case pixelSize of
					Pixel32bit -> DXGI_FORMAT_R10G10B10A2_TYPELESS
					Pixel64bit -> DXGI_FORMAT_R16G16B16A16_TYPELESS
					Pixel128bit -> DXGI_FORMAT_R32G32B32A32_TYPELESS
					_ -> DXGI_FORMAT_UNKNOWN
				PixelUint -> case pixelSize of
					Pixel32bit -> case colorSpace of
						LinearColorSpace -> DXGI_FORMAT_R8G8B8A8_UNORM
						StandardColorSpace -> DXGI_FORMAT_R8G8B8A8_UNORM_SRGB
					Pixel64bit -> DXGI_FORMAT_R16G16B16A16_UNORM
					_ -> DXGI_FORMAT_UNKNOWN
				PixelFloat -> case pixelSize of
					Pixel64bit -> DXGI_FORMAT_R16G16B16A16_FLOAT
					Pixel128bit -> DXGI_FORMAT_R32G32B32A32_FLOAT
					_ -> DXGI_FORMAT_UNKNOWN
	CompressedTextureFormat
		{ textureFormatCompression = compression
		, textureFormatColorSpace = colorSpace
		} -> case compression of
			TextureCompressionBC1 -> case colorSpace of
				LinearColorSpace -> DXGI_FORMAT_BC1_UNORM
				StandardColorSpace -> DXGI_FORMAT_BC1_UNORM_SRGB
			TextureCompressionBC1Alpha -> DXGI_FORMAT_UNKNOWN
			TextureCompressionBC2 -> case colorSpace of
				LinearColorSpace -> DXGI_FORMAT_BC2_UNORM
				StandardColorSpace -> DXGI_FORMAT_BC2_UNORM_SRGB
			TextureCompressionBC3 -> case colorSpace of
				LinearColorSpace -> DXGI_FORMAT_BC3_UNORM
				StandardColorSpace -> DXGI_FORMAT_BC3_UNORM_SRGB
			TextureCompressionBC4 -> DXGI_FORMAT_BC4_UNORM
			TextureCompressionBC4Signed -> DXGI_FORMAT_BC4_SNORM
			TextureCompressionBC5 -> DXGI_FORMAT_BC5_UNORM
			TextureCompressionBC5Signed -> DXGI_FORMAT_BC5_SNORM