{-|
Module: Flaw.Graphics.DXGI
Description: DXGI integration.
License: MIT
-}

{-# LANGUAGE TemplateHaskell #-}

module Flaw.Graphics.DXGI
	( LUID(..)
	, DXGI_FORMAT(..)
	, DXGI_MODE_SCANLINE_ORDER(..)
	, DXGI_MODE_SCALING(..)
	, DXGI_MODE_ROTATION(..)
	, DXGI_SWAP_EFFECT(..)
	, DXGI_CPU_ACCESS(..)
	, DXGI_USAGE(..)
	, DXGI_RATIONAL(..)
	, DXGI_RGB(..)
	, DXGI_GAMMA_CONTROL_CAPABILITIES(..)
	, DXGI_GAMMA_CONTROL(..)
	, DXGI_FRAME_STATISTICS(..)
	, DXGI_SAMPLE_DESC(..)
	, DXGI_MAPPED_RECT(..)
	, DXGI_SURFACE_DESC(..)
	, DXGI_MODE_DESC(..)
	, DXGI_OUTPUT_DESC(..)
	, DXGI_SWAP_CHAIN_DESC(..)
	, DXGI_ADAPTER_DESC(..)
	, IDXGIObject(..), IDXGIObject_Class(..)
	, IDXGIDeviceSubObject(..), IDXGIDeviceSubObject_Class(..)
	, IDXGISurface(..), IDXGISurface_Class(..)
	, IDXGIOutput(..), IDXGIOutput_Class(..)
	, IDXGISwapChain(..), IDXGISwapChain_Class(..)
	, IDXGIAdapter(..), IDXGIAdapter_Class(..)
	, IDXGIFactory(..), IDXGIFactory_Class(..)
	, createDXGIFactory
	) where

import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import Flaw.FFI
import Flaw.FFI.COM
import Flaw.FFI.COM.TH
import Flaw.FFI.Win32

type HMONITOR = HANDLE

genStruct "LUID"
	[ ([t|DWORD|], "LowPart", 0)
	, ([t|LONG|], "HighPart", 0)
	]

-- | DXGI_FORMAT
genEnum [t|Word32|] "DXGI_FORMAT"
	[ ("DXGI_FORMAT_UNKNOWN", 0)
	, ("DXGI_FORMAT_R32G32B32A32_TYPELESS", 1)
	, ("DXGI_FORMAT_R32G32B32A32_FLOAT", 2)
	, ("DXGI_FORMAT_R32G32B32A32_UINT", 3)
	, ("DXGI_FORMAT_R32G32B32A32_SINT", 4)
	, ("DXGI_FORMAT_R32G32B32_TYPELESS", 5)
	, ("DXGI_FORMAT_R32G32B32_FLOAT", 6)
	, ("DXGI_FORMAT_R32G32B32_UINT", 7)
	, ("DXGI_FORMAT_R32G32B32_SINT", 8)
	, ("DXGI_FORMAT_R16G16B16A16_TYPELESS", 9)
	, ("DXGI_FORMAT_R16G16B16A16_FLOAT", 10)
	, ("DXGI_FORMAT_R16G16B16A16_UNORM", 11)
	, ("DXGI_FORMAT_R16G16B16A16_UINT", 12)
	, ("DXGI_FORMAT_R16G16B16A16_SNORM", 13)
	, ("DXGI_FORMAT_R16G16B16A16_SINT", 14)
	, ("DXGI_FORMAT_R32G32_TYPELESS", 15)
	, ("DXGI_FORMAT_R32G32_FLOAT", 16)
	, ("DXGI_FORMAT_R32G32_UINT", 17)
	, ("DXGI_FORMAT_R32G32_SINT", 18)
	, ("DXGI_FORMAT_R32G8X24_TYPELESS", 19)
	, ("DXGI_FORMAT_D32_FLOAT_S8X24_UINT", 20)
	, ("DXGI_FORMAT_R32_FLOAT_X8X24_TYPELESS", 21)
	, ("DXGI_FORMAT_X32_TYPELESS_G8X24_UINT", 22)
	, ("DXGI_FORMAT_R10G10B10A2_TYPELESS", 23)
	, ("DXGI_FORMAT_R10G10B10A2_UNORM", 24)
	, ("DXGI_FORMAT_R10G10B10A2_UINT", 25)
	, ("DXGI_FORMAT_R11G11B10_FLOAT", 26)
	, ("DXGI_FORMAT_R8G8B8A8_TYPELESS", 27)
	, ("DXGI_FORMAT_R8G8B8A8_UNORM", 28)
	, ("DXGI_FORMAT_R8G8B8A8_UNORM_SRGB", 29)
	, ("DXGI_FORMAT_R8G8B8A8_UINT", 30)
	, ("DXGI_FORMAT_R8G8B8A8_SNORM", 31)
	, ("DXGI_FORMAT_R8G8B8A8_SINT", 32)
	, ("DXGI_FORMAT_R16G16_TYPELESS", 33)
	, ("DXGI_FORMAT_R16G16_FLOAT", 34)
	, ("DXGI_FORMAT_R16G16_UNORM", 35)
	, ("DXGI_FORMAT_R16G16_UINT", 36)
	, ("DXGI_FORMAT_R16G16_SNORM", 37)
	, ("DXGI_FORMAT_R16G16_SINT", 38)
	, ("DXGI_FORMAT_R32_TYPELESS", 39)
	, ("DXGI_FORMAT_D32_FLOAT", 40)
	, ("DXGI_FORMAT_R32_FLOAT", 41)
	, ("DXGI_FORMAT_R32_UINT", 42)
	, ("DXGI_FORMAT_R32_SINT", 43)
	, ("DXGI_FORMAT_R24G8_TYPELESS", 44)
	, ("DXGI_FORMAT_D24_UNORM_S8_UINT", 45)
	, ("DXGI_FORMAT_R24_UNORM_X8_TYPELESS", 46)
	, ("DXGI_FORMAT_X24_TYPELESS_G8_UINT", 47)
	, ("DXGI_FORMAT_R8G8_TYPELESS", 48)
	, ("DXGI_FORMAT_R8G8_UNORM", 49)
	, ("DXGI_FORMAT_R8G8_UINT", 50)
	, ("DXGI_FORMAT_R8G8_SNORM", 51)
	, ("DXGI_FORMAT_R8G8_SINT", 52)
	, ("DXGI_FORMAT_R16_TYPELESS", 53)
	, ("DXGI_FORMAT_R16_FLOAT", 54)
	, ("DXGI_FORMAT_D16_UNORM", 55)
	, ("DXGI_FORMAT_R16_UNORM", 56)
	, ("DXGI_FORMAT_R16_UINT", 57)
	, ("DXGI_FORMAT_R16_SNORM", 58)
	, ("DXGI_FORMAT_R16_SINT", 59)
	, ("DXGI_FORMAT_R8_TYPELESS", 60)
	, ("DXGI_FORMAT_R8_UNORM", 61)
	, ("DXGI_FORMAT_R8_UINT", 62)
	, ("DXGI_FORMAT_R8_SNORM", 63)
	, ("DXGI_FORMAT_R8_SINT", 64)
	, ("DXGI_FORMAT_A8_UNORM", 65)
	, ("DXGI_FORMAT_R1_UNORM", 66)
	, ("DXGI_FORMAT_R9G9B9E5_SHAREDEXP", 67)
	, ("DXGI_FORMAT_R8G8_B8G8_UNORM", 68)
	, ("DXGI_FORMAT_G8R8_G8B8_UNORM", 69)
	, ("DXGI_FORMAT_BC1_TYPELESS", 70)
	, ("DXGI_FORMAT_BC1_UNORM", 71)
	, ("DXGI_FORMAT_BC1_UNORM_SRGB", 72)
	, ("DXGI_FORMAT_BC2_TYPELESS", 73)
	, ("DXGI_FORMAT_BC2_UNORM", 74)
	, ("DXGI_FORMAT_BC2_UNORM_SRGB", 75)
	, ("DXGI_FORMAT_BC3_TYPELESS", 76)
	, ("DXGI_FORMAT_BC3_UNORM", 77)
	, ("DXGI_FORMAT_BC3_UNORM_SRGB", 78)
	, ("DXGI_FORMAT_BC4_TYPELESS", 79)
	, ("DXGI_FORMAT_BC4_UNORM", 80)
	, ("DXGI_FORMAT_BC4_SNORM", 81)
	, ("DXGI_FORMAT_BC5_TYPELESS", 82)
	, ("DXGI_FORMAT_BC5_UNORM", 83)
	, ("DXGI_FORMAT_BC5_SNORM", 84)
	, ("DXGI_FORMAT_B5G6R5_UNORM", 85)
	, ("DXGI_FORMAT_B5G5R5A1_UNORM", 86)
	, ("DXGI_FORMAT_B8G8R8A8_UNORM", 87)
	, ("DXGI_FORMAT_B8G8R8X8_UNORM", 88)
	, ("DXGI_FORMAT_R10G10B10_XR_BIAS_A2_UNORM", 89)
	, ("DXGI_FORMAT_B8G8R8A8_TYPELESS", 90)
	, ("DXGI_FORMAT_B8G8R8A8_UNORM_SRGB", 91)
	, ("DXGI_FORMAT_B8G8R8X8_TYPELESS", 92)
	, ("DXGI_FORMAT_B8G8R8X8_UNORM_SRGB", 93)
	, ("DXGI_FORMAT_BC6H_TYPELESS", 94)
	, ("DXGI_FORMAT_BC6H_UF16", 95)
	, ("DXGI_FORMAT_BC6H_SF16", 96)
	, ("DXGI_FORMAT_BC7_TYPELESS", 97)
	, ("DXGI_FORMAT_BC7_UNORM", 98)
	, ("DXGI_FORMAT_BC7_UNORM_SRGB", 99)
	]

-- | DXGI_MODE_SCANLINE_ORDER
genEnum [t|Word32|] "DXGI_MODE_SCANLINE_ORDER"
	[ ("DXGI_MODE_SCANLINE_ORDER_UNSPECIFIED", 0)
	, ("DXGI_MODE_SCANLINE_ORDER_PROGRESSIVE", 1)
	, ("DXGI_MODE_SCANLINE_ORDER_UPPER_FIELD_FIRST", 2)
	, ("DXGI_MODE_SCANLINE_ORDER_LOWER_FIELD_FIRST", 3)
	]

-- | DXGI_MODE_SCALING
genEnum [t|Word32|] "DXGI_MODE_SCALING"
	[ ("DXGI_MODE_SCALING_UNSPECIFIED", 0)
	, ("DXGI_MODE_SCALING_CENTERED", 1)
	, ("DXGI_MODE_SCALING_STRETCHED", 2)
	]

-- | DXGI_MODE_ROTATION
genEnum [t|Word32|] "DXGI_MODE_ROTATION"
	[ ("DXGI_MODE_ROTATION_UNSPECIFIED", 0)
	, ("DXGI_MODE_ROTATION_IDENTITY", 1)
	, ("DXGI_MODE_ROTATION_ROTATE90", 2)
	, ("DXGI_MODE_ROTATION_ROTATE180", 3)
	, ("DXGI_MODE_ROTATION_ROTATE270", 4)
	]

-- | DXGI_SWAP_EFFECT
genEnum [t|Word32|] "DXGI_SWAP_EFFECT"
	[ ("DXGI_SWAP_EFFECT_DISCARD", 0)
	, ("DXGI_SWAP_EFFECT_SEQUENTIAL", 1)
	, ("DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL", 3)
	]

-- | DXGI_CPU_ACCESS_* flags
genEnum [t|Word32|] "DXGI_CPU_ACCESS"
	[ ("DXGI_CPU_ACCESS_NONE", 0)
	, ("DXGI_CPU_ACCESS_DYNAMIC", 1)
	, ("DXGI_CPU_ACCESS_READ_WRITE", 2)
	, ("DXGI_CPU_ACCESS_SCRATCH", 3)
	, ("DXGI_CPU_ACCESS_FIELD", 15)
	]

-- | DXGI_USAGE_* flags
genEnum [t|Word32|] "DXGI_USAGE"
	[ ("DXGI_USAGE_SHADER_INPUT", 16)
	, ("DXGI_USAGE_RENDER_TARGET_OUTPUT", 32)
	, ("DXGI_USAGE_BACK_BUFFER", 64)
	, ("DXGI_USAGE_SHARED", 128)
	, ("DXGI_USAGE_READ_ONLY", 256)
	, ("DXGI_USAGE_DISCARD_ON_PRESENT", 512)
	, ("DXGI_USAGE_UNORDERED_ACCESS", 1024)
	]

-- | DXGI_RATIONAL
genStruct "DXGI_RATIONAL"
	[ ([t|UINT|], "Numerator", 0)
	, ([t|UINT|], "Denominator", 0)
	]

-- | DXGI_RGB
genStruct "DXGI_RGB"
	[ ([t|Float|], "Red", 0)
	, ([t|Float|], "Green", 0)
	, ([t|Float|], "Blue", 0)
	]

-- | DXGI_GAMMA_CONTROL_CAPABILITIES
genStruct "DXGI_GAMMA_CONTROL_CAPABILITIES"
	[ ([t|BOOL|], "ScaleAndOffsetSupported", 0)
	, ([t|Float|], "MaxConvertedValue", 0)
	, ([t|Float|], "MinConvertedValue", 0)
	, ([t|UINT|], "NumGammaControlPoints", 0)
	, ([t|Float|], "ControlPointPositions", 0)
	]

-- | DXGI_GAMMA_CONTROL
genStruct "DXGI_GAMMA_CONTROL"
	[ ([t|DXGI_RGB|], "Scale", 0)
	, ([t|DXGI_RGB|], "Offset", 0)
	, ([t|DXGI_RGB|], "GammaCurve", 1025)
	]

-- | DXGI_FRAME_STATISTICS
genStruct "DXGI_FRAME_STATISTICS"
	[ ([t|UINT|], "PresentCount", 0)
	, ([t|UINT|], "PresentRefreshCount", 0)
	, ([t|UINT|], "SyncRefreshCount", 0)
	, ([t|LARGE_INTEGER|], "SyncQPCTime", 0)
	, ([t|LARGE_INTEGER|], "SyncGPUTime", 0)
	]

-- | DXGI_SAMPLE_DESC
genStruct "DXGI_SAMPLE_DESC"
	[ ([t|UINT|], "Count", 0)
	, ([t|UINT|], "Quality", 0)
	]

-- | DXGI_MAPPED_RECT
genStruct "DXGI_MAPPED_RECT"
	[ ([t|INT|], "Pitch", 0)
	, ([t|Ptr BYTE|], "pBits", 0)
	]

-- | DXGI_SURFACE_DESC
genStruct "DXGI_SURFACE_DESC"
	[ ([t|UINT|], "Width", 0)
	, ([t|UINT|], "Height", 0)
	, ([t|DXGI_FORMAT|], "Format", 0)
	, ([t|DXGI_SAMPLE_DESC|], "SampleDesc", 0)
	]

-- | DXGI_MODE_DESC
genStruct "DXGI_MODE_DESC"
	[ ([t|UINT|], "Width", 0)
	, ([t|UINT|], "Height", 0)
	, ([t|DXGI_RATIONAL|], "RefreshRate", 0)
	, ([t|DXGI_FORMAT|], "Format", 0)
	, ([t|DXGI_MODE_SCANLINE_ORDER|], "ScanlineOrdering", 0)
	, ([t|DXGI_MODE_SCALING|], "Scaling", 0)
	]

-- | DXGI_OUTPUT_DESC
genStruct "DXGI_OUTPUT_DESC"
	[ ([t|WCHAR|], "DeviceName", 32)
	, ([t|RECT|], "DesktopCoordinates", 0)
	, ([t|BOOL|], "AttachedToDesktop", 0)
	, ([t|DXGI_MODE_ROTATION|], "Rotation", 0)
	, ([t|HMONITOR|], "Monitor", 0)
	]

-- | DXGI_SWAP_CHAIN_DESC
genStruct "DXGI_SWAP_CHAIN_DESC"
	[ ([t|DXGI_MODE_DESC|], "BufferDesc", 0)
	, ([t|DXGI_SAMPLE_DESC|], "SampleDesc", 0)
	, ([t|DXGI_USAGE|], "BufferUsage", 0)
	, ([t|UINT|], "BufferCount", 0)
	, ([t|HWND|], "OutputWindow", 0)
	, ([t|BOOL|], "Windowed", 0)
	, ([t|DXGI_SWAP_EFFECT|], "SwapEffect", 0)
	, ([t|UINT|], "Flags", 0)
	]

-- | DXGI_ADAPTER_DESC
genStruct "DXGI_ADAPTER_DESC"
	[ ([t|WCHAR|], "Description", 128)
	, ([t|UINT|], "VendorId", 0)
	, ([t|UINT|], "DeviceId", 0)
	, ([t|UINT|], "SubSysId", 0)
	, ([t|UINT|], "Revision", 0)
	, ([t|SIZE_T|], "DedicatedVideoMemory", 0)
	, ([t|SIZE_T|], "DedicatedSystemMemory", 0)
	, ([t|SIZE_T|], "SharedSystemMemory", 0)
	, ([t|LUID|], "AdapterLuid", 0)
	]

-- | IDXGIObject
genCOMInterface "IDXGIObject" "aec22fb8-76f3-4639-9be0-28eb43a67a2e" (Just "IUnknown")
	[ ([t| REFGUID -> UINT -> Ptr () -> IO HRESULT |], "SetPrivateData")
	, ([t| REFGUID -> Ptr () -> IO HRESULT |], "SetPrivateDataInterface")
	, ([t| REFGUID -> Ptr UINT -> Ptr () -> IO HRESULT |], "GetPrivateData")
	, ([t| REFIID -> Ptr (Ptr ()) -> IO HRESULT |], "GetParent")
	]

-- | IDXGIDeviceSubObject
genCOMInterface "IDXGIDeviceSubObject" "3d3e0379-f9de-4d58-bb6c-18d62992f1a6" (Just "IDXGIObject")
	[ ([t| REFIID -> Ptr (Ptr ()) -> IO HRESULT |], "GetDevice")
	]

-- | IDXGISurface
genCOMInterface "IDXGISurface" "cafcb56c-6ac3-4889-bf47-9e23bbd260ec" (Just "IDXGIDeviceSubObject")
	[ ([t| Ptr DXGI_SURFACE_DESC -> IO HRESULT |], "GetDesc")
	, ([t| Ptr DXGI_MAPPED_RECT -> UINT -> IO HRESULT |], "Map")
	, ([t| IO HRESULT |], "Unmap")
	]

-- | IDXGIOutput
genCOMInterface "IDXGIOutput" "ae02eedb-c735-4690-8d52-5a8dc20213aa" (Just "IDXGIObject")
	[ ([t| Ptr DXGI_OUTPUT_DESC -> IO HRESULT |], "GetDesc")
	, ([t| EnumWrapper DXGI_FORMAT -> UINT -> Ptr UINT -> Ptr DXGI_MODE_DESC -> IO HRESULT |], "GetDisplayModeList")
	, ([t| Ptr DXGI_MODE_DESC -> Ptr DXGI_MODE_DESC -> Ptr IUnknown -> IO HRESULT |], "FindClosestMatchingMode")
	, ([t| IO HRESULT |], "WaitForVBlank")
	, ([t| Ptr IUnknown -> BOOL -> IO HRESULT |], "TakeOwnership")
	, ([t| IO () |], "ReleaseOwnership")
	, ([t| Ptr DXGI_GAMMA_CONTROL_CAPABILITIES -> IO HRESULT |], "GetGammaControlCapabilities")
	, ([t| Ptr DXGI_GAMMA_CONTROL -> IO HRESULT |], "SetGammaControl")
	, ([t| Ptr DXGI_GAMMA_CONTROL -> IO HRESULT |], "GetGammaControl")
	, ([t| Ptr IDXGISurface -> IO HRESULT |], "SetDisplaySurface")
	, ([t| Ptr IDXGISurface -> IO HRESULT |], "GetDisplaySurfaceData")
	, ([t| Ptr DXGI_FRAME_STATISTICS -> IO HRESULT |], "GetFrameStatistics")
	]

-- | IDXGISwapChain
genCOMInterface "IDXGISwapChain" "310d36a0-d2e7-4c0a-aa04-6a9d23b8886a" (Just "IDXGIDeviceSubObject")
	[ ([t| UINT -> UINT -> IO HRESULT |], "Present")
	, ([t| UINT -> REFIID -> Ptr (Ptr ()) -> IO HRESULT |], "GetBuffer")
	, ([t| BOOL -> Ptr IDXGIOutput -> IO HRESULT |], "SetFullscreenState")
	, ([t| Ptr BOOL -> Ptr (Ptr IDXGIOutput) -> IO HRESULT |], "GetFullscreenState")
	, ([t| Ptr DXGI_SWAP_CHAIN_DESC -> IO HRESULT |], "GetDesc")
	, ([t| UINT -> UINT -> UINT -> EnumWrapper DXGI_FORMAT -> UINT -> IO HRESULT |], "ResizeBuffers")
	, ([t| Ptr DXGI_MODE_DESC -> IO HRESULT |], "ResizeTarget")
	, ([t| Ptr (Ptr IDXGIOutput) -> IO HRESULT |], "GetContainingOutput")
	, ([t| Ptr DXGI_FRAME_STATISTICS -> IO HRESULT |], "GetFrameStatistics")
	, ([t| Ptr UINT -> IO HRESULT |], "GetLastPresentCount")
	]

-- | IDXGIAdapter
genCOMInterface "IDXGIAdapter" "2411e7e1-12ac-4ccf-bd14-9798e8534dc0" (Just "IDXGIObject")
	[ ([t| UINT -> Ptr (Ptr IDXGIOutput) -> IO HRESULT |], "EnumOutputs")
	, ([t| Ptr DXGI_ADAPTER_DESC -> IO HRESULT |], "GetDesc")
	, ([t| Ptr GUID -> Ptr LARGE_INTEGER -> IO HRESULT |], "CheckInterfaceSupport")
	]

-- | IDXGIFactory
genCOMInterface "IDXGIFactory" "7b7166ec-21c7-44ae-b21a-c9ae321ae369" (Just "IDXGIObject")
	[ ([t| UINT -> Ptr (Ptr IDXGIAdapter) -> IO HRESULT |], "EnumAdapters")
	, ([t| HWND -> UINT -> IO HRESULT |], "MakeWindowAssociation")
	, ([t| Ptr HWND -> IO HRESULT |], "GetWindowAssociation")
	, ([t| Ptr IUnknown -> Ptr DXGI_SWAP_CHAIN_DESC -> Ptr (Ptr IDXGISwapChain) -> IO HRESULT |], "CreateSwapChain")
	, ([t| HMODULE -> Ptr (Ptr IDXGIAdapter) -> IO HRESULT |], "CreateSoftwareAdapter")
	]

-- | Wrapper for CreateDXGIFactory
createDXGIFactory :: IO IDXGIFactory
createDXGIFactory = do
	dll <- loadLibrary "dxgi.dll"
	if dll == nullPtr then fail "no dxgi.dll"
	else do
		proc <- getProcAddress dll "CreateDXGIFactory"
		if proc == nullPtr then fail "wrong dxgi.dll"
		else alloca $ \factoryPtr -> do
			hr <- alloca $ \iidPtr -> do
				poke iidPtr (getIID (undefined :: IDXGIFactory))
				mkCreateDXGIFactory (castPtrToFunPtr proc) iidPtr factoryPtr
			if hresultFailed hr then fail "cannot create DXGI factory"
			else peekCOMObject . castPtr =<< peek factoryPtr

type CreateDXGIFactoryProc = REFIID -> Ptr (Ptr ()) -> IO HRESULT
foreign import stdcall safe "dynamic" mkCreateDXGIFactory :: FunPtr CreateDXGIFactoryProc -> CreateDXGIFactoryProc
