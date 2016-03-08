{-|
Module: Flaw.Graphics.DirectX11.FFI
Description: FFI for DirectX 11.
License: MIT
-}

{-# LANGUAGE TemplateHaskell #-}

module Flaw.Graphics.DirectX11.FFI
	( D3D_DRIVER_TYPE(..)
	, D3D_FEATURE_LEVEL(..)
	, D3D11_USAGE(..)
	, D3D11_BIND_FLAG(..)
	, D3D11_CPU_ACCESS_FLAG(..)
	, D3D11_SRV_DIMENSION(..)
	, D3D11_RTV_DIMENSION(..)
	, D3D11_DSV_DIMENSION(..)
	, D3D11_UAV_DIMENSION(..)
	, D3D11_RESOURCE_DIMENSION(..)
	, D3D11_MAP(..)
	, D3D11_CLEAR_FLAG(..)
	, D3D11_PRIMITIVE_TOPOLOGY(..)
	, D3D11_DEVICE_CONTEXT_TYPE(..)
	, D3D11_INPUT_CLASSIFICATION(..)
	, D3D11_COMPARISON_FUNC(..)
	, D3D11_DEPTH_WRITE_MASK(..)
	, D3D11_STENCIL_OP(..)
	, D3D11_BLEND(..)
	, D3D11_BLEND_OP(..)
	, D3D11_FILL_MODE(..)
	, D3D11_CULL_MODE(..)
	, D3D11_FILTER(..)
	, D3D11_TEXTURE_ADDRESS_MODE(..)
	, D3D11_COUNTER(..)
	, D3D11_COUNTER_TYPE(..)
	, D3D11_QUERY(..)
	, D3D11_FEATURE(..)
	, D3DCOMPILE_FLAGS(..)
	, D3D11_INPUT_ELEMENT_DESC(..)
	, D3D11_SO_DECLARATION_ENTRY(..)
	, D3D11_BUFFER_DESC(..)
	, D3D11_SUBRESOURCE_DATA(..)
	, D3D11_MAPPED_SUBRESOURCE(..)
	, D3D11_VIEWPORT(..)
	, D3D11_RECT
	, D3D11_BOX(..)
	, D3D11_RENDER_TARGET_BLEND_DESC(..)
	, D3D11_BLEND_DESC(..)
	, D3D11_DEPTH_STENCILOP_DESC(..)
	, D3D11_DEPTH_STENCIL_DESC(..)
	, D3D11_RASTERIZER_DESC(..)
	, D3D11_SAMPLER_DESC(..)
	, D3D11_QUERY_DESC(..)
	, D3D11_COUNTER_DESC(..)
	, D3D11_COUNTER_INFO(..)
	, D3D11_CLASS_INSTANCE_DESC(..)
	, D3D11_TEXTURE1D_DESC(..)
	, D3D11_TEXTURE2D_DESC(..)
	, D3D11_TEXTURE3D_DESC(..)
	, D3D11_BUFFER_SRV(..)
	, D3D11_TEX1D_SRV(..)
	, D3D11_TEX1D_ARRAY_SRV(..)
	, D3D11_TEX2D_SRV(..)
	, D3D11_TEX2D_ARRAY_SRV(..)
	, D3D11_TEX2DMS_SRV(..)
	, D3D11_TEX2DMS_ARRAY_SRV(..)
	, D3D11_TEX3D_SRV(..)
	, D3D11_TEXCUBE_SRV(..)
	, D3D11_TEXCUBE_ARRAY_SRV(..)
	, D3D11_BUFFEREX_SRV(..)
	, D3D11_SHADER_RESOURCE_VIEW_DESC(..)
	, D3D11_BUFFER_RTV(..)
	, D3D11_TEX1D_RTV(..)
	, D3D11_TEX1D_ARRAY_RTV(..)
	, D3D11_TEX2D_RTV(..)
	, D3D11_TEX2DMS_RTV(..)
	, D3D11_TEX2D_ARRAY_RTV(..)
	, D3D11_TEX2DMS_ARRAY_RTV(..)
	, D3D11_TEX3D_RTV(..)
	, D3D11_RENDER_TARGET_VIEW_DESC(..)
	, D3D11_TEX1D_DSV(..)
	, D3D11_TEX1D_ARRAY_DSV(..)
	, D3D11_TEX2D_DSV(..)
	, D3D11_TEX2D_ARRAY_DSV(..)
	, D3D11_TEX2DMS_DSV(..)
	, D3D11_TEX2DMS_ARRAY_DSV(..)
	, D3D11_DEPTH_STENCIL_VIEW_DESC(..)
	, D3D11_BUFFER_UAV(..)
	, D3D11_TEX1D_UAV(..)
	, D3D11_TEX1D_ARRAY_UAV(..)
	, D3D11_TEX2D_UAV(..)
	, D3D11_TEX2D_ARRAY_UAV(..)
	, D3D11_TEX3D_UAV(..)
	, D3D11_UNORDERED_ACCESS_VIEW_DESC(..)
	, ID3DBlob(..), ID3DBlob_Class(..)
	, ID3D11DeviceChild(..), ID3D11DeviceChild_Class(..)
	, ID3D11Asynchronous(..), ID3D11Asynchronous_Class(..)
	, ID3D11Counter(..), ID3D11Counter_Class(..)
	, ID3D11Query(..), ID3D11Query_Class(..)
	, ID3D11Predicate(..), ID3D11Predicate_Class(..)
	, ID3D11SamplerState(..), ID3D11SamplerState_Class(..)
	, ID3D11RasterizerState(..), ID3D11RasterizerState_Class(..)
	, ID3D11DepthStencilState(..), ID3D11DepthStencilState_Class(..)
	, ID3D11BlendState(..), ID3D11BlendState_Class(..)
	, ID3D11ClassLinkage(..), ID3D11ClassLinkage_Class(..)
	, ID3D11ClassInstance(..), ID3D11ClassInstance_Class(..)
	, ID3D11InputLayout(..), ID3D11InputLayout_Class(..)
	, ID3D11VertexShader(..), ID3D11VertexShader_Class(..)
	, ID3D11PixelShader(..), ID3D11PixelShader_Class(..)
	, ID3D11GeometryShader(..), ID3D11GeometryShader_Class(..)
	, ID3D11HullShader(..), ID3D11HullShader_Class(..)
	, ID3D11DomainShader(..), ID3D11DomainShader_Class(..)
	, ID3D11ComputeShader(..), ID3D11ComputeShader_Class(..)
	, ID3D11CommandList(..), ID3D11CommandList_Class(..)
	, ID3D11Resource(..), ID3D11Resource_Class(..)
	, ID3D11Buffer(..), ID3D11Buffer_Class(..)
	, ID3D11Texture1D(..), ID3D11Texture1D_Class(..)
	, ID3D11Texture2D(..), ID3D11Texture2D_Class(..)
	, ID3D11Texture3D(..), ID3D11Texture3D_Class(..)
	, ID3D11View(..), ID3D11View_Class(..)
	, ID3D11ShaderResourceView(..), ID3D11ShaderResourceView_Class(..)
	, ID3D11RenderTargetView(..), ID3D11RenderTargetView_Class(..)
	, ID3D11DepthStencilView(..), ID3D11DepthStencilView_Class(..)
	, ID3D11UnorderedAccessView(..), ID3D11UnorderedAccessView_Class(..)
	, ID3D11DeviceContext(..), ID3D11DeviceContext_Class(..)
	, ID3D11Device(..), ID3D11Device_Class(..)
	, d3d11CreateDevice
	, d3d11SdkVersion
	, D3DCompileProc
	, mkD3DCompile
	) where

import Data.Bits
import Data.Word
import Foreign.Ptr

import Flaw.FFI
import Flaw.FFI.COM
import Flaw.FFI.COM.TH
import Flaw.FFI.Win32
import Flaw.Graphics.DXGI.FFI

------- Enums

-- | D3D_DRIVER_TYPE
genEnum [t|Word32|] "D3D_DRIVER_TYPE"
	[ ("D3D_DRIVER_TYPE_UNKNOWN", 0)
	, ("D3D_DRIVER_TYPE_HARDWARE", 1)
	, ("D3D_DRIVER_TYPE_REFERENCE", 2)
	, ("D3D_DRIVER_TYPE_NULL", 3)
	, ("D3D_DRIVER_TYPE_SOFTWARE", 4)
	, ("D3D_DRIVER_TYPE_WARP", 5)
	]

-- | D3D_FEATURE_LEVEL
genEnum [t|Word32|] "D3D_FEATURE_LEVEL"
	[ ("D3D_FEATURE_LEVEL_9_1", 0x9100)
	, ("D3D_FEATURE_LEVEL_9_2", 0x9200)
	, ("D3D_FEATURE_LEVEL_9_3", 0x9300)
	, ("D3D_FEATURE_LEVEL_10_0", 0xa000)
	, ("D3D_FEATURE_LEVEL_10_1", 0xa100)
	, ("D3D_FEATURE_LEVEL_11_0", 0xb000)
	, ("D3D_FEATURE_LEVEL_11_1", 0xb100)
	]

-- | D3D11_USAGE
genEnum [t|Word32|] "D3D11_USAGE"
	[ ("D3D11_USAGE_DEFAULT", 0)
	, ("D3D11_USAGE_IMMUTABLE", 1)
	, ("D3D11_USAGE_DYNAMIC", 2)
	, ("D3D11_USAGE_STAGING", 3)
	]

-- | D3D11_BIND_FLAG
genEnum [t|Word32|] "D3D11_BIND_FLAG"
	[ ("D3D11_BIND_VERTEX_BUFFER", 1)
	, ("D3D11_BIND_INDEX_BUFFER", 2)
	, ("D3D11_BIND_CONSTANT_BUFFER", 4)
	, ("D3D11_BIND_SHADER_RESOURCE", 8)
	, ("D3D11_BIND_STREAM_OUTPUT", 16)
	, ("D3D11_BIND_RENDER_TARGET", 32)
	, ("D3D11_BIND_DEPTH_STENCIL", 64)
	, ("D3D11_BIND_UNORDERED_ACCESS", 128)
	, ("D3D11_BIND_DECODER", 256)
	, ("D3D11_BIND_VIDEO_ENCODER", 512)
	]

-- | D3D11_CPU_ACCESS_FLAG
genEnum [t|Word32|] "D3D11_CPU_ACCESS_FLAG"
	[ ("D3D11_CPU_ACCESS_WRITE", 0x10000)
	, ("D3D11_CPU_ACCESS_READ", 0x20000)
	]

-- | D3D11_SRV_DIMENSION
genEnum [t|Word32|] "D3D11_SRV_DIMENSION"
	[ ("D3D11_SRV_DIMENSION_UNKNOWN", 0)
	, ("D3D11_SRV_DIMENSION_BUFFER", 1)
	, ("D3D11_SRV_DIMENSION_TEXTURE1D", 2)
	, ("D3D11_SRV_DIMENSION_TEXTURE1DARRAY", 3)
	, ("D3D11_SRV_DIMENSION_TEXTURE2D", 4)
	, ("D3D11_SRV_DIMENSION_TEXTURE2DARRAY", 5)
	, ("D3D11_SRV_DIMENSION_TEXTURE2DMS", 6)
	, ("D3D11_SRV_DIMENSION_TEXTURE2DMSARRAY", 7)
	, ("D3D11_SRV_DIMENSION_TEXTURE3D", 8)
	, ("D3D11_SRV_DIMENSION_TEXTURECUBE", 9)
	, ("D3D11_SRV_DIMENSION_TEXTURECUBEARRAY", 10)
	, ("D3D11_SRV_DIMENSION_BUFFEREX", 11)
	]

-- | D3D11_RTV_DIMENSION
genEnum [t|Word32|] "D3D11_RTV_DIMENSION"
	[ ("D3D11_RTV_DIMENSION_UNKNOWN", 0)
	, ("D3D11_RTV_DIMENSION_BUFFER", 1)
	, ("D3D11_RTV_DIMENSION_TEXTURE1D", 2)
	, ("D3D11_RTV_DIMENSION_TEXTURE1DARRAY", 3)
	, ("D3D11_RTV_DIMENSION_TEXTURE2D", 4)
	, ("D3D11_RTV_DIMENSION_TEXTURE2DARRAY", 5)
	, ("D3D11_RTV_DIMENSION_TEXTURE2DMS", 6)
	, ("D3D11_RTV_DIMENSION_TEXTURE2DMSARRAY", 7)
	, ("D3D11_RTV_DIMENSION_TEXTURE3D", 8)
	]

-- | D3D11_DSV_DIMENSION
genEnum [t|Word32|] "D3D11_DSV_DIMENSION"
	[ ("D3D11_DSV_DIMENSION_UNKNOWN", 0)
	, ("D3D11_DSV_DIMENSION_TEXTURE1D", 1)
	, ("D3D11_DSV_DIMENSION_TEXTURE1DARRAY", 2)
	, ("D3D11_DSV_DIMENSION_TEXTURE2D", 3)
	, ("D3D11_DSV_DIMENSION_TEXTURE2DARRAY", 4)
	, ("D3D11_DSV_DIMENSION_TEXTURE2DMS", 5)
	, ("D3D11_DSV_DIMENSION_TEXTURE2DMSARRAY", 6)
	]

-- | D3D11_UAV_DIMENSION
genEnum [t|Word32|] "D3D11_UAV_DIMENSION"
	[ ("D3D11_UAV_DIMENSION_UNKNOWN", 0)
	, ("D3D11_UAV_DIMENSION_BUFFER", 1)
	, ("D3D11_UAV_DIMENSION_TEXTURE1D", 2)
	, ("D3D11_UAV_DIMENSION_TEXTURE1DARRAY", 3)
	, ("D3D11_UAV_DIMENSION_TEXTURE2D", 4)
	, ("D3D11_UAV_DIMENSION_TEXTURE2DARRAY", 5)
	, ("D3D11_UAV_DIMENSION_TEXTURE3D", 8)
	]

-- | D3D11_RESOURCE_DIMENSION
genEnum [t|Word32|] "D3D11_RESOURCE_DIMENSION"
	[ ("D3D11_RESOURCE_DIMENSION_UNKNOWN", 0)
	, ("D3D11_RESOURCE_DIMENSION_BUFFER", 1)
	, ("D3D11_RESOURCE_DIMENSION_TEXTURE1D", 2)
	, ("D3D11_RESOURCE_DIMENSION_TEXTURE2D", 3)
	, ("D3D11_RESOURCE_DIMENSION_TEXTURE3D", 4)
	]

-- | D3D11_MAP
genEnum [t|Word32|] "D3D11_MAP"
	[ ("D3D11_MAP_READ", 1)
	, ("D3D11_MAP_WRITE", 2)
	, ("D3D11_MAP_READ_WRITE", 3)
	, ("D3D11_MAP_WRITE_DISCARD", 4)
	, ("D3D11_MAP_WRITE_NO_OVERWRITE", 5)
	]

-- | D3D11_CLEAR_FLAG
genEnum [t|Word32|] "D3D11_CLEAR_FLAG"
	[ ("D3D11_CLEAR_DEPTH", 1)
	, ("D3D11_CLEAR_STENCIL", 2)
	]

-- | D3D11_PRIMITIVE_TOPOLOGY
genEnum [t|Word32|] "D3D11_PRIMITIVE_TOPOLOGY"
	[ ("D3D11_PRIMITIVE_TOPOLOGY_UNDEFINED", 0)
	, ("D3D11_PRIMITIVE_TOPOLOGY_POINTLIST", 1)
	, ("D3D11_PRIMITIVE_TOPOLOGY_LINELIST", 2)
	, ("D3D11_PRIMITIVE_TOPOLOGY_LINESTRIP", 3)
	, ("D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST", 4)
	, ("D3D11_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP", 5)
	, ("D3D11_PRIMITIVE_TOPOLOGY_LINELIST_ADJ", 10)
	, ("D3D11_PRIMITIVE_TOPOLOGY_LINESTRIP_ADJ", 11)
	, ("D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST_ADJ", 12)
	, ("D3D11_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP_ADJ", 13)
	, ("D3D11_PRIMITIVE_TOPOLOGY_1_CONTROL_POINT_PATCHLIST", 33)
	, ("D3D11_PRIMITIVE_TOPOLOGY_2_CONTROL_POINT_PATCHLIST", 34)
	, ("D3D11_PRIMITIVE_TOPOLOGY_3_CONTROL_POINT_PATCHLIST", 35)
	, ("D3D11_PRIMITIVE_TOPOLOGY_4_CONTROL_POINT_PATCHLIST", 36)
	, ("D3D11_PRIMITIVE_TOPOLOGY_5_CONTROL_POINT_PATCHLIST", 37)
	, ("D3D11_PRIMITIVE_TOPOLOGY_6_CONTROL_POINT_PATCHLIST", 38)
	, ("D3D11_PRIMITIVE_TOPOLOGY_7_CONTROL_POINT_PATCHLIST", 39)
	, ("D3D11_PRIMITIVE_TOPOLOGY_8_CONTROL_POINT_PATCHLIST", 40)
	, ("D3D11_PRIMITIVE_TOPOLOGY_9_CONTROL_POINT_PATCHLIST", 41)
	, ("D3D11_PRIMITIVE_TOPOLOGY_10_CONTROL_POINT_PATCHLIST", 42)
	, ("D3D11_PRIMITIVE_TOPOLOGY_11_CONTROL_POINT_PATCHLIST", 43)
	, ("D3D11_PRIMITIVE_TOPOLOGY_12_CONTROL_POINT_PATCHLIST", 44)
	, ("D3D11_PRIMITIVE_TOPOLOGY_13_CONTROL_POINT_PATCHLIST", 45)
	, ("D3D11_PRIMITIVE_TOPOLOGY_14_CONTROL_POINT_PATCHLIST", 46)
	, ("D3D11_PRIMITIVE_TOPOLOGY_15_CONTROL_POINT_PATCHLIST", 47)
	, ("D3D11_PRIMITIVE_TOPOLOGY_16_CONTROL_POINT_PATCHLIST", 48)
	, ("D3D11_PRIMITIVE_TOPOLOGY_17_CONTROL_POINT_PATCHLIST", 49)
	, ("D3D11_PRIMITIVE_TOPOLOGY_18_CONTROL_POINT_PATCHLIST", 50)
	, ("D3D11_PRIMITIVE_TOPOLOGY_19_CONTROL_POINT_PATCHLIST", 51)
	, ("D3D11_PRIMITIVE_TOPOLOGY_20_CONTROL_POINT_PATCHLIST", 52)
	, ("D3D11_PRIMITIVE_TOPOLOGY_21_CONTROL_POINT_PATCHLIST", 53)
	, ("D3D11_PRIMITIVE_TOPOLOGY_22_CONTROL_POINT_PATCHLIST", 54)
	, ("D3D11_PRIMITIVE_TOPOLOGY_23_CONTROL_POINT_PATCHLIST", 55)
	, ("D3D11_PRIMITIVE_TOPOLOGY_24_CONTROL_POINT_PATCHLIST", 56)
	, ("D3D11_PRIMITIVE_TOPOLOGY_25_CONTROL_POINT_PATCHLIST", 57)
	, ("D3D11_PRIMITIVE_TOPOLOGY_26_CONTROL_POINT_PATCHLIST", 58)
	, ("D3D11_PRIMITIVE_TOPOLOGY_27_CONTROL_POINT_PATCHLIST", 59)
	, ("D3D11_PRIMITIVE_TOPOLOGY_28_CONTROL_POINT_PATCHLIST", 60)
	, ("D3D11_PRIMITIVE_TOPOLOGY_29_CONTROL_POINT_PATCHLIST", 61)
	, ("D3D11_PRIMITIVE_TOPOLOGY_30_CONTROL_POINT_PATCHLIST", 62)
	, ("D3D11_PRIMITIVE_TOPOLOGY_31_CONTROL_POINT_PATCHLIST", 63)
	, ("D3D11_PRIMITIVE_TOPOLOGY_32_CONTROL_POINT_PATCHLIST", 64)
	]

-- | D3D11_DEVICE_CONTEXT_TYPE
genEnum [t|Word32|] "D3D11_DEVICE_CONTEXT_TYPE"
	[ ("D3D11_DEVICE_CONTEXT_IMMEDIATE", 0)
	, ("D3D11_DEVICE_CONTEXT_DEFERRED", 1)
	]

-- | D3D11_INPUT_CLASSIFICATION
genEnum [t|Word32|] "D3D11_INPUT_CLASSIFICATION"
	[ ("D3D11_INPUT_PER_VERTEX_DATA", 0)
	, ("D3D11_INPUT_PER_INSTANCE_DATA", 1)
	]

-- | D3D11_COMPARISON_FUNC
genEnum [t|Word32|] "D3D11_COMPARISON_FUNC"
	[ ("D3D11_COMPARISON_NEVER", 1)
	, ("D3D11_COMPARISON_LESS", 2)
	, ("D3D11_COMPARISON_EQUAL", 3)
	, ("D3D11_COMPARISON_LESS_EQUAL", 4)
	, ("D3D11_COMPARISON_GREATER", 5)
	, ("D3D11_COMPARISON_NOT_EQUAL", 6)
	, ("D3D11_COMPARISON_GREATER_EQUAL", 7)
	, ("D3D11_COMPARISON_ALWAYS", 8)
	]

-- | D3D11_DEPTH_WRITE_MASK
genEnum [t|Word32|] "D3D11_DEPTH_WRITE_MASK"
	[ ("D3D11_DEPTH_WRITE_MASK_ZERO", 0)
	, ("D3D11_DEPTH_WRITE_MASK_ALL", 1)
	]

-- | D3D11_STENCIL_OP
genEnum [t|Word32|] "D3D11_STENCIL_OP"
	[ ("D3D11_STENCIL_OP_KEEP", 1)
	, ("D3D11_STENCIL_OP_ZERO", 2)
	, ("D3D11_STENCIL_OP_REPLACE", 3)
	, ("D3D11_STENCIL_OP_INCR_SAT", 4)
	, ("D3D11_STENCIL_OP_DECR_SAT", 5)
	, ("D3D11_STENCIL_OP_INVERT", 6)
	, ("D3D11_STENCIL_OP_INCR", 7)
	, ("D3D11_STENCIL_OP_DECR", 8)
	]

-- | D3D11_BLEND
genEnum [t|Word32|] "D3D11_BLEND"
	[ ("D3D11_BLEND_ZERO", 1)
	, ("D3D11_BLEND_ONE", 2)
	, ("D3D11_BLEND_SRC_COLOR", 3)
	, ("D3D11_BLEND_INV_SRC_COLOR", 4)
	, ("D3D11_BLEND_SRC_ALPHA", 5)
	, ("D3D11_BLEND_INV_SRC_ALPHA", 6)
	, ("D3D11_BLEND_DEST_ALPHA", 7)
	, ("D3D11_BLEND_INV_DEST_ALPHA", 8)
	, ("D3D11_BLEND_DEST_COLOR", 9)
	, ("D3D11_BLEND_INV_DEST_COLOR", 10)
	, ("D3D11_BLEND_SRC_ALPHA_SAT", 11)
	, ("D3D11_BLEND_BLEND_FACTOR", 14)
	, ("D3D11_BLEND_INV_BLEND_FACTOR", 15)
	, ("D3D11_BLEND_SRC1_COLOR", 16)
	, ("D3D11_BLEND_INV_SRC1_COLOR", 17)
	, ("D3D11_BLEND_SRC1_ALPHA", 18)
	, ("D3D11_BLEND_INV_SRC1_ALPHA", 19)
	]

-- | D3D11_BLEND_OP
genEnum [t|Word32|] "D3D11_BLEND_OP"
	[ ("D3D11_BLEND_OP_ADD", 1)
	, ("D3D11_BLEND_OP_SUBTRACT", 2)
	, ("D3D11_BLEND_OP_REV_SUBTRACT", 3)
	, ("D3D11_BLEND_OP_MIN", 4)
	, ("D3D11_BLEND_OP_MAX", 5)
	]

-- | D3D11_FILL_MODE
genEnum [t|Word32|] "D3D11_FILL_MODE"
	[ ("D3D11_FILL_WIREFRAME", 2)
	, ("D3D11_FILL_SOLID", 3)
	]

-- | D3D11_CULL_MODE
genEnum [t|Word32|] "D3D11_CULL_MODE"
	[ ("D3D11_CULL_NONE", 1)
	, ("D3D11_CULL_FRONT", 2)
	, ("D3D11_CULL_BACK", 3)
	]

-- | D3D11_FILTER
genEnum [t|Word32|] "D3D11_FILTER"
	[ ("D3D11_FILTER_MIN_MAG_MIP_POINT", 0)
	, ("D3D11_FILTER_MIN_MAG_POINT_MIP_LINEAR", 0x1)
	, ("D3D11_FILTER_MIN_POINT_MAG_LINEAR_MIP_POINT", 0x4)
	, ("D3D11_FILTER_MIN_POINT_MAG_MIP_LINEAR", 0x5)
	, ("D3D11_FILTER_MIN_LINEAR_MAG_MIP_POINT", 0x10)
	, ("D3D11_FILTER_MIN_LINEAR_MAG_POINT_MIP_LINEAR", 0x11)
	, ("D3D11_FILTER_MIN_MAG_LINEAR_MIP_POINT", 0x14)
	, ("D3D11_FILTER_MIN_MAG_MIP_LINEAR", 0x15)
	, ("D3D11_FILTER_ANISOTROPIC", 0x55)
	, ("D3D11_FILTER_COMPARISON_MIN_MAG_MIP_POINT", 0x80)
	, ("D3D11_FILTER_COMPARISON_MIN_MAG_POINT_MIP_LINEAR", 0x81)
	, ("D3D11_FILTER_COMPARISON_MIN_POINT_MAG_LINEAR_MIP_POINT", 0x84)
	, ("D3D11_FILTER_COMPARISON_MIN_POINT_MAG_MIP_LINEAR", 0x85)
	, ("D3D11_FILTER_COMPARISON_MIN_LINEAR_MAG_MIP_POINT", 0x90)
	, ("D3D11_FILTER_COMPARISON_MIN_LINEAR_MAG_POINT_MIP_LINEAR", 0x91)
	, ("D3D11_FILTER_COMPARISON_MIN_MAG_LINEAR_MIP_POINT", 0x94)
	, ("D3D11_FILTER_COMPARISON_MIN_MAG_MIP_LINEAR", 0x95)
	, ("D3D11_FILTER_COMPARISON_ANISOTROPIC", 0xd5)
	, ("D3D11_FILTER_MINIMUM_MIN_MAG_MIP_POINT", 0x100)
	, ("D3D11_FILTER_MINIMUM_MIN_MAG_POINT_MIP_LINEAR", 0x101)
	, ("D3D11_FILTER_MINIMUM_MIN_POINT_MAG_LINEAR_MIP_POINT", 0x104)
	, ("D3D11_FILTER_MINIMUM_MIN_POINT_MAG_MIP_LINEAR", 0x105)
	, ("D3D11_FILTER_MINIMUM_MIN_LINEAR_MAG_MIP_POINT", 0x110)
	, ("D3D11_FILTER_MINIMUM_MIN_LINEAR_MAG_POINT_MIP_LINEAR", 0x111)
	, ("D3D11_FILTER_MINIMUM_MIN_MAG_LINEAR_MIP_POINT", 0x114)
	, ("D3D11_FILTER_MINIMUM_MIN_MAG_MIP_LINEAR", 0x115)
	, ("D3D11_FILTER_MINIMUM_ANISOTROPIC", 0x155)
	, ("D3D11_FILTER_MAXIMUM_MIN_MAG_MIP_POINT", 0x180)
	, ("D3D11_FILTER_MAXIMUM_MIN_MAG_POINT_MIP_LINEAR", 0x181)
	, ("D3D11_FILTER_MAXIMUM_MIN_POINT_MAG_LINEAR_MIP_POINT", 0x184)
	, ("D3D11_FILTER_MAXIMUM_MIN_POINT_MAG_MIP_LINEAR", 0x185)
	, ("D3D11_FILTER_MAXIMUM_MIN_LINEAR_MAG_MIP_POINT", 0x190)
	, ("D3D11_FILTER_MAXIMUM_MIN_LINEAR_MAG_POINT_MIP_LINEAR", 0x191)
	, ("D3D11_FILTER_MAXIMUM_MIN_MAG_LINEAR_MIP_POINT", 0x194)
	, ("D3D11_FILTER_MAXIMUM_MIN_MAG_MIP_LINEAR", 0x195)
	, ("D3D11_FILTER_MAXIMUM_ANISOTROPIC", 0x1d5)
	]

-- | D3D11_TEXTURE_ADDRESS_MODE
genEnum [t|Word32|] "D3D11_TEXTURE_ADDRESS_MODE"
	[ ("D3D11_TEXTURE_ADDRESS_WRAP", 1)
	, ("D3D11_TEXTURE_ADDRESS_MIRROR", 2)
	, ("D3D11_TEXTURE_ADDRESS_CLAMP", 3)
	, ("D3D11_TEXTURE_ADDRESS_BORDER", 4)
	, ("D3D11_TEXTURE_ADDRESS_MIRROR_ONCE", 5)
	]

-- | D3D11_COUNTER
genEnum [t|Word32|] "D3D11_COUNTER"
	[ ("D3D11_COUNTER_DEVICE_DEPENDENT_0", 0x40000000)
	]

-- | D3D11_COUNTER_TYPE
genEnum [t|Word32|] "D3D11_COUNTER_TYPE"
	[ ("D3D11_COUNTER_TYPE_FLOAT32", 0)
	, ("D3D11_COUNTER_TYPE_UINT16", 1)
	, ("D3D11_COUNTER_TYPE_UINT32", 2)
	, ("D3D11_COUNTER_TYPE_UINT64", 3)
	]

-- | D3D11_QUERY
genEnum [t|Word32|] "D3D11_QUERY"
	[ ("D3D11_QUERY_EVENT", 0)
	, ("D3D11_QUERY_OCCLUSION", 1)
	, ("D3D11_QUERY_TIMESTAMP", 2)
	, ("D3D11_QUERY_TIMESTAMP_DISJOINT", 3)
	, ("D3D11_QUERY_PIPELINE_STATISTICS", 4)
	, ("D3D11_QUERY_OCCLUSION_PREDICATE", 5)
	, ("D3D11_QUERY_SO_STATISTICS", 6)
	, ("D3D11_QUERY_SO_OVERFLOW_PREDICATE", 7)
	, ("D3D11_QUERY_SO_STATISTICS_STREAM0", 8)
	, ("D3D11_QUERY_SO_OVERFLOW_PREDICATE_STREAM0", 9)
	, ("D3D11_QUERY_SO_STATISTICS_STREAM1", 10)
	, ("D3D11_QUERY_SO_OVERFLOW_PREDICATE_STREAM1", 11)
	, ("D3D11_QUERY_SO_STATISTICS_STREAM2", 12)
	, ("D3D11_QUERY_SO_OVERFLOW_PREDICATE_STREAM2", 13)
	, ("D3D11_QUERY_SO_STATISTICS_STREAM3", 14)
	, ("D3D11_QUERY_SO_OVERFLOW_PREDICATE_STREAM3", 15)
	]

-- | D3D11_FEATURE
genEnum [t|Word32|] "D3D11_FEATURE"
	[ ("D3D11_FEATURE_THREADING", 0)
	, ("D3D11_FEATURE_DOUBLES", 1)
	, ("D3D11_FEATURE_FORMAT_SUPPORT", 2)
	, ("D3D11_FEATURE_FORMAT_SUPPORT2", 3)
	, ("D3D11_FEATURE_D3D10_X_HARDWARE_OPTIONS", 4)
	, ("D3D11_FEATURE_D3D11_OPTIONS", 5)
	, ("D3D11_FEATURE_ARCHITECTURE_INFO", 6)
	, ("D3D11_FEATURE_D3D9_OPTIONS", 7)
	, ("D3D11_FEATURE_SHADER_MIN_PRECISION_SUPPORT", 8)
	, ("D3D11_FEATURE_D3D9_SHADOW_SUPPORT", 9)
	, ("D3D11_FEATURE_D3D11_OPTIONS1", 10)
	, ("D3D11_FEATURE_D3D9_SIMPLE_INSTANCING_SUPPORT", 11)
	, ("D3D11_FEATURE_MARKER_SUPPORT", 12)
	, ("D3D11_FEATURE_D3D9_OPTIONS1", 13)
	]

-- | D3DCOMPILE_FLAGS
genEnum [t|Word32|] "D3DCOMPILE_FLAGS"
	[ ("D3DCOMPILE_DEBUG", (1 `shiftL` 0))
	, ("D3DCOMPILE_SKIP_VALIDATION", (1 `shiftL` 1))
	, ("D3DCOMPILE_SKIP_OPTIMIZATION", (1 `shiftL` 2))
	, ("D3DCOMPILE_PACK_MATRIX_ROW_MAJOR", (1 `shiftL` 3))
	, ("D3DCOMPILE_PACK_MATRIX_COLUMN_MAJOR", (1 `shiftL` 4))
	, ("D3DCOMPILE_PARTIAL_PRECISION", (1 `shiftL` 5))
	, ("D3DCOMPILE_FORCE_VS_SOFTWARE_NO_OPT", (1 `shiftL` 6))
	, ("D3DCOMPILE_FORCE_PS_SOFTWARE_NO_OPT", (1 `shiftL` 7))
	, ("D3DCOMPILE_NO_PRESHADER", (1 `shiftL` 8))
	, ("D3DCOMPILE_AVOID_FLOW_CONTROL", (1 `shiftL` 9))
	, ("D3DCOMPILE_PREFER_FLOW_CONTROL", (1 `shiftL` 10))
	, ("D3DCOMPILE_ENABLE_STRICTNESS", (1 `shiftL` 11))
	, ("D3DCOMPILE_ENABLE_BACKWARDS_COMPATIBILITY", (1 `shiftL` 12))
	, ("D3DCOMPILE_IEEE_STRICTNESS", (1 `shiftL` 13))
	, ("D3DCOMPILE_OPTIMIZATION_LEVEL0", (1 `shiftL` 14))
	, ("D3DCOMPILE_OPTIMIZATION_LEVEL1", 0)
	, ("D3DCOMPILE_OPTIMIZATION_LEVEL2", ((1 `shiftL` 14) .|. (1 `shiftL` 15)))
	, ("D3DCOMPILE_OPTIMIZATION_LEVEL3", (1 `shiftL` 15))
	, ("D3DCOMPILE_RESERVED16", (1 `shiftL` 16))
	, ("D3DCOMPILE_RESERVED17", (1 `shiftL` 17))
	, ("D3DCOMPILE_WARNINGS_ARE_ERRORS", (1 `shiftL` 18))
	, ("D3DCOMPILE_RESOURCES_MAY_ALIAS", (1 `shiftL` 19))
	]

------- Structs

-- | D3D11_INPUT_ELEMENT_DESC
genStruct "D3D11_INPUT_ELEMENT_DESC"
	[ ([t|LPSTR|], "SemanticName")
	, ([t|UINT|], "SemanticIndex")
	, ([t|DXGI_FORMAT|], "Format")
	, ([t|UINT|], "InputSlot")
	, ([t|UINT|], "AlignedByteOffset")
	, ([t|D3D11_INPUT_CLASSIFICATION|], "InputSlotClass")
	, ([t|UINT|], "InstanceDataStepRate")
	]

-- | D3D11_SO_DECLARATION_ENTRY
genStruct "D3D11_SO_DECLARATION_ENTRY"
	[ ([t|UINT|], "Stream")
	, ([t|LPSTR|], "SemanticName")
	, ([t|UINT|], "SemanticIndex")
	, ([t|BYTE|], "StartComponent")
	, ([t|BYTE|], "ComponentCount")
	, ([t|BYTE|], "OutputSlot")
	]

-- | D3D11_BUFFER_DESC
genStruct "D3D11_BUFFER_DESC"
	[ ([t|UINT|], "ByteWidth")
	, ([t|D3D11_USAGE|], "Usage")
	, ([t|UINT|], "BindFlags")
	, ([t|UINT|], "CPUAccessFlags")
	, ([t|UINT|], "MiscFlags")
	, ([t|UINT|], "StructureByteStride")
	]

-- | D3D11_SUBRESOURCE_DATA
genStruct "D3D11_SUBRESOURCE_DATA"
	[ ([t|Ptr ()|], "pSysMem")
	, ([t|UINT|], "SysMemPitch")
	, ([t|UINT|], "SysMemSlicePitch")
	]

-- | D3D11_MAPPED_SUBRESOURCE
genStruct "D3D11_MAPPED_SUBRESOURCE"
	[ ([t|Ptr ()|], "pData")
	, ([t|UINT|], "RowPitch")
	, ([t|UINT|], "DepthPitch")
	]

-- | D3D11_VIEWPORT
genStruct "D3D11_VIEWPORT"
	[ ([t|FLOAT|], "TopLeftX")
	, ([t|FLOAT|], "TopLeftY")
	, ([t|FLOAT|], "Width")
	, ([t|FLOAT|], "Height")
	, ([t|FLOAT|], "MinDepth")
	, ([t|FLOAT|], "MaxDepth")
	]

-- | D3D11_RECT
type D3D11_RECT = RECT

-- | D3D11_BOX
genStruct "D3D11_BOX"
	[ ([t|UINT|], "left")
	, ([t|UINT|], "top")
	, ([t|UINT|], "front")
	, ([t|UINT|], "right")
	, ([t|UINT|], "bottom")
	, ([t|UINT|], "back")
	]

-- | D3D11_RENDER_TARGET_BLEND_DESC
genStruct "D3D11_RENDER_TARGET_BLEND_DESC"
	[ ([t|BOOL|], "BlendEnable")
	, ([t|D3D11_BLEND|], "SrcBlend")
	, ([t|D3D11_BLEND|], "DestBlend")
	, ([t|D3D11_BLEND_OP|], "BlendOp")
	, ([t|D3D11_BLEND|], "SrcBlendAlpha")
	, ([t|D3D11_BLEND|], "DestBlendAlpha")
	, ([t|D3D11_BLEND_OP|], "BlendOpAlpha")
	, ([t|UINT8|], "RenderTargetWriteMask")
	]

-- | D3D11_BLEND_DESC
genStructWithArrays "D3D11_BLEND_DESC"
	[ ([t|BOOL|], "AlphaToCoverageEnable", 0)
	, ([t|BOOL|], "IndependentBlendEnable", 0)
	, ([t|D3D11_RENDER_TARGET_BLEND_DESC|], "RenderTarget", 8)
	]

-- | D3D11_DEPTH_STENCILOP_DESC
genStruct "D3D11_DEPTH_STENCILOP_DESC"
	[ ([t|D3D11_STENCIL_OP|], "StencilFailOp")
	, ([t|D3D11_STENCIL_OP|], "StencilDepthFailOp")
	, ([t|D3D11_STENCIL_OP|], "StencilPassOp")
	, ([t|D3D11_COMPARISON_FUNC|], "StencilFunc")
	]

-- | D3D11_DEPTH_STENCIL_DESC
genStruct "D3D11_DEPTH_STENCIL_DESC"
	[ ([t|BOOL|], "DepthEnable")
	, ([t|D3D11_DEPTH_WRITE_MASK|], "DepthWriteMask")
	, ([t|D3D11_COMPARISON_FUNC|], "DepthFunc")
	, ([t|BOOL|], "StencilEnable")
	, ([t|UINT8|], "StencilReadMask")
	, ([t|UINT8|], "StencilWriteMask")
	, ([t|D3D11_DEPTH_STENCILOP_DESC|], "FrontFace")
	, ([t|D3D11_DEPTH_STENCILOP_DESC|], "BackFace")
	]

-- | D3D11_RASTERIZER_DESC
genStruct "D3D11_RASTERIZER_DESC"
	[ ([t|D3D11_FILL_MODE|], "FillMode")
	, ([t|D3D11_CULL_MODE|], "CullMode")
	, ([t|BOOL|], "FrontCounterClockwise")
	, ([t|INT|], "DepthBias")
	, ([t|FLOAT|], "DepthBiasClamp")
	, ([t|FLOAT|], "SlopeScaledDepthBias")
	, ([t|BOOL|], "DepthClipEnable")
	, ([t|BOOL|], "ScissorEnable")
	, ([t|BOOL|], "MultisampleEnable")
	, ([t|BOOL|], "AntialiasedLineEnable")
	]

-- | D3D11_SAMPLER_DESC
genStructWithArrays "D3D11_SAMPLER_DESC"
	[ ([t|D3D11_FILTER|], "Filter", 0)
	, ([t|D3D11_TEXTURE_ADDRESS_MODE|], "AddressU", 0)
	, ([t|D3D11_TEXTURE_ADDRESS_MODE|], "AddressV", 0)
	, ([t|D3D11_TEXTURE_ADDRESS_MODE|], "AddressW", 0)
	, ([t|FLOAT|], "MipLODBias", 0)
	, ([t|UINT|], "MaxAnisotropy", 0)
	, ([t|D3D11_COMPARISON_FUNC|], "ComparisonFunc", 0)
	, ([t|FLOAT|], "BorderColor", 4)
	, ([t|FLOAT|], "MinLOD", 0)
	, ([t|FLOAT|], "MaxLOD", 0)
	]

-- | D3D11_QUERY_DESC
genStruct "D3D11_QUERY_DESC"
	[ ([t|D3D11_QUERY|], "Query")
	, ([t|UINT|], "MiscFlags")
	]

-- | D3D11_COUNTER_DESC
genStruct "D3D11_COUNTER_DESC"
	[ ([t|D3D11_COUNTER|], "Counter")
	, ([t|UINT|], "MiscFlags")
	]

-- | D3D11_COUNTER_INFO
genStruct "D3D11_COUNTER_INFO"
	[ ([t|D3D11_COUNTER|], "LastDeviceDependentCounter")
	, ([t|UINT|], "NumSimultaneousCounters")
	, ([t|UINT8|], "NumDetectableParallelUnits")
	]

-- | D3D11_CLASS_INSTANCE_DESC
genStruct "D3D11_CLASS_INSTANCE_DESC"
	[ ([t|UINT|], "InstanceId")
	, ([t|UINT|], "InstanceIndex")
	, ([t|UINT|], "TypeId")
	, ([t|UINT|], "ConstantBuffer")
	, ([t|UINT|], "BaseConstantBufferOffset")
	, ([t|UINT|], "BaseTexture")
	, ([t|UINT|], "BaseSampler")
	, ([t|BOOL|], "Created")
	]

-- | D3D11_TEXTURE1D_DESC
genStruct "D3D11_TEXTURE1D_DESC"
	[ ([t|UINT|], "Width")
	, ([t|UINT|], "MipLevels")
	, ([t|UINT|], "ArraySize")
	, ([t|DXGI_FORMAT|], "Format")
	, ([t|D3D11_USAGE|], "Usage")
	, ([t|UINT|], "BindFlags")
	, ([t|UINT|], "CPUAccessFlags")
	, ([t|UINT|], "MiscFlags")
	]

-- | D3D11_TEXTURE2D_DESC
genStruct "D3D11_TEXTURE2D_DESC"
	[ ([t|UINT|], "Width")
	, ([t|UINT|], "Height")
	, ([t|UINT|], "MipLevels")
	, ([t|UINT|], "ArraySize")
	, ([t|DXGI_FORMAT|], "Format")
	, ([t|DXGI_SAMPLE_DESC|], "SampleDesc")
	, ([t|D3D11_USAGE|], "Usage")
	, ([t|UINT|], "BindFlags")
	, ([t|UINT|], "CPUAccessFlags")
	, ([t|UINT|], "MiscFlags")
	]

-- | D3D11_TEXTURE3D_DESC
genStruct "D3D11_TEXTURE3D_DESC"
	[ ([t|UINT|], "Width")
	, ([t|UINT|], "Height")
	, ([t|UINT|], "Depth")
	, ([t|UINT|], "MipLevels")
	, ([t|DXGI_FORMAT|], "Format")
	, ([t|D3D11_USAGE|], "Usage")
	, ([t|UINT|], "BindFlags")
	, ([t|UINT|], "CPUAccessFlags")
	, ([t|UINT|], "MiscFlags")
	]

-- | D3D11_BUFFER_SRV
genStruct "D3D11_BUFFER_SRV"
	[ ([t|UINT|], "FirstElementOrElementOffset")
	, ([t|UINT|], "NumElementsOrElementWidth")
	]

-- | D3D11_TEX1D_SRV
genStruct "D3D11_TEX1D_SRV"
	[ ([t|UINT|], "MostDetailedMip")
	, ([t|UINT|], "MipLevels")
	]

-- | D3D11_TEX1D_ARRAY_SRV
genStruct "D3D11_TEX1D_ARRAY_SRV"
	[ ([t|UINT|], "MostDetailedMip")
	, ([t|UINT|], "MipLevels")
	, ([t|UINT|], "FirstArraySlice")
	, ([t|UINT|], "ArraySize")
	]

-- | D3D11_TEX2D_SRV
genStruct "D3D11_TEX2D_SRV"
	[ ([t|UINT|], "MostDetailedMip")
	, ([t|UINT|], "MipLevels")
	]

-- | D3D11_TEX2D_ARRAY_SRV
genStruct "D3D11_TEX2D_ARRAY_SRV"
	[ ([t|UINT|], "MostDetailedMip")
	, ([t|UINT|], "MipLevels")
	, ([t|UINT|], "FirstArraySlice")
	, ([t|UINT|], "ArraySize")
	]

-- | D3D11_TEX2DMS_SRV
genStruct "D3D11_TEX2DMS_SRV"
	[ ([t|UINT|], "UnusedField_NothingToDefine")
	]

-- | D3D11_TEX2DMS_ARRAY_SRV
genStruct "D3D11_TEX2DMS_ARRAY_SRV"
	[ ([t|UINT|], "FirstArraySlice")
	, ([t|UINT|], "ArraySize")
	]

-- | D3D11_TEX3D_SRV
genStruct "D3D11_TEX3D_SRV"
	[ ([t|UINT|], "MostDetailedMip")
	, ([t|UINT|], "MipLevels")
	]

-- | D3D11_TEXCUBE_SRV
genStruct "D3D11_TEXCUBE_SRV"
	[ ([t|UINT|], "MostDetailedMip")
	, ([t|UINT|], "MipLevels")
	]

-- | D3D11_TEXCUBE_ARRAY_SRV
genStruct "D3D11_TEXCUBE_ARRAY_SRV"
	[ ([t|UINT|], "MostDetailedMip")
	, ([t|UINT|], "MipLevels")
	, ([t|UINT|], "First2DArrayFace")
	, ([t|UINT|], "NumCubes")
	]

-- | D3D11_BUFFEREX_SRV
genStruct "D3D11_BUFFEREX_SRV"
	[ ([t|UINT|], "FirstElement")
	, ([t|UINT|], "NumElements")
	, ([t|UINT|], "Flags")
	]

-- | D3D11_SHADER_RESOURCE_VIEW_DESC
genStructWithEndUnion "D3D11_SHADER_RESOURCE_VIEW_DESC"
	[ ([t|DXGI_FORMAT|], "Format", 0)
	, ([t|D3D11_SRV_DIMENSION|], "ViewDimension", 0)
	] 1
	[ ("D3D11_SRV_DIMENSION_UNKNOWN", [t|Int|], "Unknown")
	, ("D3D11_SRV_DIMENSION_BUFFER", [t|D3D11_BUFFER_SRV|], "Buffer")
	, ("D3D11_SRV_DIMENSION_TEXTURE1D", [t|D3D11_TEX1D_SRV|], "Texture1D")
	, ("D3D11_SRV_DIMENSION_TEXTURE1DARRAY", [t|D3D11_TEX1D_ARRAY_SRV|], "Texture1DArray")
	, ("D3D11_SRV_DIMENSION_TEXTURE2D", [t|D3D11_TEX2D_SRV|], "Texture2D")
	, ("D3D11_SRV_DIMENSION_TEXTURE2DARRAY", [t|D3D11_TEX2D_ARRAY_SRV|], "Texture2DArray")
	, ("D3D11_SRV_DIMENSION_TEXTURE2DMS", [t|D3D11_TEX2DMS_SRV|], "Texture2DMS")
	, ("D3D11_SRV_DIMENSION_TEXTURE2DMSARRAY", [t|D3D11_TEX2DMS_ARRAY_SRV|], "Texture2DMSArray")
	, ("D3D11_SRV_DIMENSION_TEXTURE3D", [t|D3D11_TEX3D_SRV|], "Texture3D")
	, ("D3D11_SRV_DIMENSION_TEXTURECUBE", [t|D3D11_TEXCUBE_SRV|], "TextureCube")
	, ("D3D11_SRV_DIMENSION_TEXTURECUBEARRAY", [t|D3D11_TEXCUBE_ARRAY_SRV|], "TextureCubeArray")
	, ("D3D11_SRV_DIMENSION_BUFFEREX", [t|D3D11_BUFFEREX_SRV|], "BufferEx")
	]

-- | D3D11_BUFFER_RTV
genStruct "D3D11_BUFFER_RTV"
	[ ([t|UINT|], "FirstElementOrElementOffset")
	, ([t|UINT|], "NumElementsOrElementWidth")
	]

-- | D3D11_TEX1D_RTV
genStruct "D3D11_TEX1D_RTV"
	[ ([t|UINT|], "MipSlice")
	]

-- | D3D11_TEX1D_ARRAY_RTV
genStruct "D3D11_TEX1D_ARRAY_RTV"
	[ ([t|UINT|], "MipSlice")
	, ([t|UINT|], "FirstArraySlice")
	, ([t|UINT|], "ArraySize")
	]

-- | D3D11_TEX2D_RTV
genStruct "D3D11_TEX2D_RTV"
	[ ([t|UINT|], "MipSlice")
	]

-- | D3D11_TEX2DMS_RTV
genStruct "D3D11_TEX2DMS_RTV"
	[ ([t|UINT|], "UnusedField_NothingToDefine")
	]

-- | D3D11_TEX2D_ARRAY_RTV
genStruct "D3D11_TEX2D_ARRAY_RTV"
	[ ([t|UINT|], "MipSlice")
	, ([t|UINT|], "FirstArraySlice")
	, ([t|UINT|], "ArraySize")
	]

-- | D3D11_TEX2DMS_ARRAY_RTV
genStruct "D3D11_TEX2DMS_ARRAY_RTV"
	[ ([t|UINT|], "FirstArraySlice")
	, ([t|UINT|], "ArraySize")
	]

-- | D3D11_TEX3D_RTV
genStruct "D3D11_TEX3D_RTV"
	[ ([t|UINT|], "MipSlice")
	, ([t|UINT|], "FirstWSlice")
	, ([t|UINT|], "WSize")
	]

-- | D3D11_RENDER_TARGET_VIEW_DESC
genStructWithEndUnion "D3D11_RENDER_TARGET_VIEW_DESC"
	[ ([t|DXGI_FORMAT|], "Format", 0)
	, ([t|D3D11_RTV_DIMENSION|], "ViewDimension", 0)
	] 1
	[ ("D3D11_RTV_DIMENSION_UNKNOWN", [t|Int|], "Unknown")
	, ("D3D11_RTV_DIMENSION_BUFFER", [t|D3D11_BUFFER_RTV|], "Buffer")
	, ("D3D11_RTV_DIMENSION_TEXTURE1D", [t|D3D11_TEX1D_RTV|], "Texture1D")
	, ("D3D11_RTV_DIMENSION_TEXTURE1DARRAY", [t|D3D11_TEX1D_ARRAY_RTV|], "Texture1DArray")
	, ("D3D11_RTV_DIMENSION_TEXTURE2D", [t|D3D11_TEX2D_RTV|], "Texture2D")
	, ("D3D11_RTV_DIMENSION_TEXTURE2DARRAY", [t|D3D11_TEX2D_ARRAY_RTV|], "Texture2DArray")
	, ("D3D11_RTV_DIMENSION_TEXTURE2DMS", [t|D3D11_TEX2DMS_RTV|], "Texture2DMS")
	, ("D3D11_RTV_DIMENSION_TEXTURE2DMSARRAY", [t|D3D11_TEX2DMS_ARRAY_RTV|], "Texture2DMSArray")
	, ("D3D11_RTV_DIMENSION_TEXTURE3D", [t|D3D11_TEX3D_RTV|], "Texture3D")
	]

-- | D3D11_TEX1D_DSV
genStruct "D3D11_TEX1D_DSV"
	[ ([t|UINT|], "MipSlice")
	]

-- | D3D11_TEX1D_ARRAY_DSV
genStruct "D3D11_TEX1D_ARRAY_DSV"
	[ ([t|UINT|], "MipSlice")
	, ([t|UINT|], "FirstArraySlice")
	, ([t|UINT|], "ArraySize")
	]

-- | D3D11_TEX2D_DSV
genStruct "D3D11_TEX2D_DSV"
	[ ([t|UINT|], "MipSlice")
	]

-- | D3D11_TEX2D_ARRAY_DSV
genStruct "D3D11_TEX2D_ARRAY_DSV"
	[ ([t|UINT|], "MipSlice")
	, ([t|UINT|], "FirstArraySlice")
	, ([t|UINT|], "ArraySize")
	]

-- | D3D11_TEX2DMS_DSV
genStruct "D3D11_TEX2DMS_DSV"
	[ ([t|UINT|], "UnusedField_NothingToDefine")
	]

-- | D3D11_TEX2DMS_ARRAY_DSV
genStruct "D3D11_TEX2DMS_ARRAY_DSV"
	[ ([t|UINT|], "FirstArraySlice")
	, ([t|UINT|], "ArraySize")
	]

-- | D3D11_DEPTH_STENCIL_VIEW_DESC
genStructWithEndUnion "D3D11_DEPTH_STENCIL_VIEW_DESC"
	[ ([t|DXGI_FORMAT|], "Format", 0)
	, ([t|D3D11_DSV_DIMENSION|], "ViewDimension", 0)
	, ([t|UINT|], "Flags", 0)
	] 1
	[ ("D3D11_DSV_DIMENSION_UNKNOWN", [t|Int|], "Unknown")
	, ("D3D11_DSV_DIMENSION_TEXTURE1D", [t|D3D11_TEX1D_DSV|], "Texture1D")
	, ("D3D11_DSV_DIMENSION_TEXTURE1DARRAY", [t|D3D11_TEX1D_ARRAY_DSV|], "Texture1DArray")
	, ("D3D11_DSV_DIMENSION_TEXTURE2D", [t|D3D11_TEX2D_DSV|], "Texture2D")
	, ("D3D11_DSV_DIMENSION_TEXTURE2DARRAY", [t|D3D11_TEX2D_ARRAY_DSV|], "Texture2DArray")
	, ("D3D11_DSV_DIMENSION_TEXTURE2DMS", [t|D3D11_TEX2DMS_DSV|], "Texture2DMS")
	, ("D3D11_DSV_DIMENSION_TEXTURE2DMSARRAY", [t|D3D11_TEX2DMS_ARRAY_DSV|], "Texture2DMSArray")
	]

-- | D3D11_BUFFER_UAV
genStruct "D3D11_BUFFER_UAV"
	[ ([t|UINT|], "FirstElement")
	, ([t|UINT|], "NumElements")
	, ([t|UINT|], "Flags")
	]

-- | D3D11_TEX1D_UAV
genStruct "D3D11_TEX1D_UAV"
	[ ([t|UINT|], "MipSlice")
	]

-- | D3D11_TEX1D_ARRAY_UAV
genStruct "D3D11_TEX1D_ARRAY_UAV"
	[ ([t|UINT|], "MipSlice")
	, ([t|UINT|], "FirstArraySlice")
	, ([t|UINT|], "ArraySize")
	]

-- | D3D11_TEX2D_UAV
genStruct "D3D11_TEX2D_UAV"
	[ ([t|UINT|], "MipSlice")
	]

-- | D3D11_TEX2D_ARRAY_UAV
genStruct "D3D11_TEX2D_ARRAY_UAV"
	[ ([t|UINT|], "MipSlice")
	, ([t|UINT|], "FirstArraySlice")
	, ([t|UINT|], "ArraySize")
	]

-- | D3D11_TEX3D_UAV
genStruct "D3D11_TEX3D_UAV"
	[ ([t|UINT|], "MipSlice")
	, ([t|UINT|], "FirstWSlice")
	, ([t|UINT|], "WSize")
	]

-- | D3D11_UNORDERED_ACCESS_VIEW_DESC
genStructWithEndUnion "D3D11_UNORDERED_ACCESS_VIEW_DESC"
	[ ([t|DXGI_FORMAT|], "Format", 0)
	, ([t|D3D11_UAV_DIMENSION|], "ViewDimension", 0)
	] 1
	[ ("D3D11_UAV_DIMENSION_UNKNOWN", [t|Int|], "Unknown")
	, ("D3D11_UAV_DIMENSION_BUFFER", [t|D3D11_BUFFER_UAV|], "Buffer")
	, ("D3D11_UAV_DIMENSION_TEXTURE1D", [t|D3D11_TEX1D_UAV|], "Texture1D")
	, ("D3D11_UAV_DIMENSION_TEXTURE1DARRAY", [t|D3D11_TEX1D_ARRAY_UAV|], "Texture1DArray")
	, ("D3D11_UAV_DIMENSION_TEXTURE2D", [t|D3D11_TEX2D_UAV|], "Texture2D")
	, ("D3D11_UAV_DIMENSION_TEXTURE2DARRAY", [t|D3D11_TEX2D_ARRAY_UAV|], "Texture2DArray")
	, ("D3D11_UAV_DIMENSION_TEXTURE3D", [t|D3D11_TEX3D_UAV|], "Texture3D")
	]

------- Interfaces

fmap concat $ sequence
	-- ID3DBlob (actually ID3D10Blob)
	[ genCOMInterface "ID3DBlob" "8BA5FB08-5195-40e2-AC58-0D989C3A0102" ["IUnknown"]
		[ ([t| IO (Ptr ()) |], "GetBufferPointer")
		, ([t| IO SIZE_T |], "GetBufferSize")
		]

	-- ID3D11DeviceChild
	, genCOMInterface "ID3D11DeviceChild" "1841e5c8-16b0-489b-bcc8-44cfb0d5deae" ["IUnknown"]
		[ ([t| Ptr (Ptr $(forwardRef "ID3D11Device")) -> IO () |], "GetDevice")
		, ([t| REFGUID -> Ptr UINT -> Ptr () -> IO HRESULT |], "GetPrivateData")
		, ([t| REFGUID -> UINT -> Ptr () -> IO HRESULT |], "SetPrivateData")
		, ([t| REFGUID -> Ptr IUnknown -> IO HRESULT |], "SetPrivateDataInterface")
		]

	-- ID3D11Asynchronous
	, genCOMInterface "ID3D11Asynchronous" "4b35d0cd-1e15-4258-9c98-1b1333f6dd3b" ["ID3D11DeviceChild", "IUnknown"]
		[ ([t| IO UINT |], "GetDataSize")
		]

	-- ID3D11Counter
	, genCOMInterface "ID3D11Counter" "6e8c49fb-a371-4770-b440-29086022b741" ["ID3D11Asynchronous", "ID3D11DeviceChild", "IUnknown"]
		[ ([t| Ptr D3D11_COUNTER_DESC -> IO () |], "GetDesc")
		]

	-- ID3D11Query
	, genCOMInterface "ID3D11Query" "d6c00747-87b7-425e-b84d-44d108560afd" ["ID3D11Asynchronous", "ID3D11DeviceChild", "IUnknown"]
		[ ([t| Ptr D3D11_QUERY_DESC -> IO () |], "GetDesc")
		]

	-- ID3D11Predicate
	, genCOMInterface "ID3D11Predicate" "9eb576dd-9f77-4d86-81aa-8bab5fe490e2" ["ID3D11Query", "ID3D11Asynchronous", "ID3D11DeviceChild", "IUnknown"]
		[
		]

	-- ID3D11SamplerState
	, genCOMInterface "ID3D11SamplerState" "da6fea51-564c-4487-9810-f0d0f9b4e3a5" ["ID3D11DeviceChild", "IUnknown"]
		[ ([t| Ptr D3D11_SAMPLER_DESC -> IO () |], "GetDesc")
		]

	-- ID3D11RasterizerState
	, genCOMInterface "ID3D11RasterizerState" "9bb4ab81-ab1a-4d8f-b506-fc04200b6ee7" ["ID3D11DeviceChild", "IUnknown"]
		[ ([t| Ptr D3D11_RASTERIZER_DESC -> IO () |], "GetDesc")
		]

	-- ID3D11DepthStencilState
	, genCOMInterface "ID3D11DepthStencilState" "03823efb-8d8f-4e1c-9aa2-f64bb2cbfdf1" ["ID3D11DeviceChild", "IUnknown"]
		[ ([t| Ptr D3D11_DEPTH_STENCIL_DESC -> IO () |], "GetDesc")
		]

	-- ID3D11BlendState
	, genCOMInterface "ID3D11BlendState" "75b68faa-347d-4159-8f45-a0640f01cd9a" ["ID3D11DeviceChild", "IUnknown"]
		[ ([t| Ptr D3D11_BLEND_DESC -> IO () |], "GetDesc")
		]

	-- ID3D11ClassLinkage
	, genCOMInterface "ID3D11ClassLinkage" "ddf57cba-9543-46e4-a12b-f207a0fe7fed" ["ID3D11DeviceChild", "IUnknown"]
		[ ([t| LPSTR -> UINT -> Ptr (Ptr $(forwardRef "ID3D11ClassInstance")) -> IO HRESULT |], "GetClassInstance")
		, ([t| LPSTR -> UINT -> UINT -> UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11ClassInstance")) -> IO HRESULT |], "CreateClassInstance")
		]

	-- ID3D11ClassInstance
	, genCOMInterface "ID3D11ClassInstance" "a6cd7faa-b0b7-4a2f-9436-8662a65797cb" ["ID3D11DeviceChild", "IUnknown"]
		[ ([t| Ptr (Ptr $(forwardRef "ID3D11ClassLinkage")) -> IO () |], "GetClassLinkage")
		, ([t| Ptr D3D11_CLASS_INSTANCE_DESC -> IO () |], "GetDesc")
		, ([t| LPSTR -> Ptr SIZE_T -> IO () |], "GetInstanceName")
		, ([t| LPSTR -> Ptr SIZE_T -> IO () |], "GetTypeName")
		]

	-- ID3D11InputLayout
	, genCOMInterface "ID3D11InputLayout" "e4819ddc-4cf0-4025-bd26-5de82a3e07b7" ["ID3D11DeviceChild", "IUnknown"]
		[
		]

	-- ID3D11VertexShader
	, genCOMInterface "ID3D11VertexShader" "3b301d64-d678-4289-8897-22f8928b72f3" ["ID3D11DeviceChild", "IUnknown"]
		[
		]

	-- ID3D11PixelShader
	, genCOMInterface "ID3D11PixelShader" "ea82e40d-51dc-4f33-93d4-db7c9125ae8c" ["ID3D11DeviceChild", "IUnknown"]
		[
		]

	-- ID3D11GeometryShader
	, genCOMInterface "ID3D11GeometryShader" "38325b96-effb-4022-ba02-2e795b70275c" ["ID3D11DeviceChild", "IUnknown"]
		[
		]

	-- ID3D11HullShader
	, genCOMInterface "ID3D11HullShader" "8e5c6061-628a-4c8e-8264-bbe45cb3d5dd" ["ID3D11DeviceChild", "IUnknown"]
		[
		]

	-- ID3D11DomainShader
	, genCOMInterface "ID3D11DomainShader" "f582c508-0f36-490c-9977-31eece268cfa" ["ID3D11DeviceChild", "IUnknown"]
		[
		]

	-- ID3D11ComputeShader
	, genCOMInterface "ID3D11ComputeShader" "4f5b196e-c2bd-495e-bd01-1fded38e4969" ["ID3D11DeviceChild", "IUnknown"]
		[
		]

	-- ID3D11CommandList
	, genCOMInterface "ID3D11CommandList" "a24bc4d1-769e-43f7-8013-98ff566c18e2" ["ID3D11DeviceChild", "IUnknown"]
		[ ([t| IO UINT |], "GetContextFlags")
		]

	-- ID3D11Resource
	, genCOMInterface "ID3D11Resource" "dc8e63f3-d12b-4952-b47b-5e45026a862d" ["ID3D11DeviceChild", "IUnknown"]
		[ ([t| Ptr D3D11_RESOURCE_DIMENSION -> IO () |], "GetType")
		, ([t| UINT -> IO () |], "SetEvictionPriority")
		, ([t| IO UINT |], "GetEvictionPriority")
		]

	-- ID3D11Buffer
	, genCOMInterface "ID3D11Buffer" "48570b85-d1ee-4fcd-a250-eb350722b037" ["ID3D11Resource", "ID3D11DeviceChild", "IUnknown"]
		[ ([t| Ptr D3D11_BUFFER_DESC -> IO () |], "GetDesc")
		]

	-- ID3D11Texture1D
	, genCOMInterface "ID3D11Texture1D" "f8fb5c27-c6b3-4f75-a4c8-439af2ef564c" ["ID3D11Resource", "ID3D11DeviceChild", "IUnknown"]
		[ ([t| Ptr D3D11_TEXTURE1D_DESC -> IO () |], "GetDesc")
		]

	-- ID3D11Texture2D
	, genCOMInterface "ID3D11Texture2D" "6f15aaf2-d208-4e89-9ab4-489535d34f9c" ["ID3D11Resource", "ID3D11DeviceChild", "IUnknown"]
		[ ([t| Ptr D3D11_TEXTURE2D_DESC -> IO () |], "GetDesc")
		]

	-- ID3D11Texture3D
	, genCOMInterface "ID3D11Texture3D" "037e866e-f56d-4357-a8af-9dabbe6e250e" ["ID3D11Resource", "ID3D11DeviceChild", "IUnknown"]
		[ ([t| Ptr D3D11_TEXTURE3D_DESC -> IO () |], "GetDesc")
		]

	-- ID3D11View
	, genCOMInterface "ID3D11View" "839d1216-bb2e-412b-b7f4-a9dbebe08ed1" ["ID3D11DeviceChild", "IUnknown"]
		[ ([t| Ptr (Ptr $(forwardRef "ID3D11Resource")) -> IO () |], "GetResource")
		]

	-- ID3D11ShaderResourceView
	, genCOMInterface "ID3D11ShaderResourceView" "b0e06fe0-8192-4e1a-b1ca-36d7414710b2" ["ID3D11View", "ID3D11DeviceChild", "IUnknown"]
		[ ([t| Ptr D3D11_SHADER_RESOURCE_VIEW_DESC -> IO () |], "GetDesc")
		]

	-- ID3D11RenderTargetView
	, genCOMInterface "ID3D11RenderTargetView" "dfdba067-0b8d-4865-875b-d7b4516cc164" ["ID3D11View", "ID3D11DeviceChild", "IUnknown"]
		[ ([t| Ptr D3D11_RENDER_TARGET_VIEW_DESC -> IO () |], "GetDesc")
		]

	-- ID3D11DepthStencilView
	, genCOMInterface "ID3D11DepthStencilView" "9fdac92a-1876-48c3-afad-25b94f84a9b6" ["ID3D11View", "ID3D11DeviceChild", "IUnknown"]
		[ ([t| Ptr D3D11_DEPTH_STENCIL_VIEW_DESC -> IO () |], "GetDesc")
		]

	-- ID3D11UnorderedAccessView
	, genCOMInterface "ID3D11UnorderedAccessView" "28acf509-7f5c-48f6-8611-f316010a6380" ["ID3D11View", "ID3D11DeviceChild", "IUnknown"]
		[ ([t| Ptr D3D11_UNORDERED_ACCESS_VIEW_DESC -> IO () |], "GetDesc")
		]

	-- ID3D11DeviceContext
	, genCOMInterface "ID3D11DeviceContext" "c0bfa96c-e089-44fb-8eaf-26f8796190da" ["ID3D11DeviceChild", "IUnknown"]
		[ ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11Buffer")) -> IO () |], "VSSetConstantBuffers")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11ShaderResourceView")) -> IO () |], "PSSetShaderResources")
		, ([t| Ptr $(forwardRef "ID3D11PixelShader") -> Ptr (Ptr $(forwardRef "ID3D11ClassInstance")) -> UINT -> IO () |], "PSSetShader")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11SamplerState")) -> IO () |], "PSSetSamplers")
		, ([t| Ptr $(forwardRef "ID3D11VertexShader") -> Ptr (Ptr $(forwardRef "ID3D11ClassInstance")) -> UINT -> IO () |], "VSSetShader")
		, ([t| UINT -> UINT -> INT -> IO () |], "DrawIndexed")
		, ([t| UINT -> UINT -> IO () |], "Draw")
		, ([t| Ptr $(forwardRef "ID3D11Resource") -> UINT -> EnumWrapper D3D11_MAP -> UINT -> Ptr D3D11_MAPPED_SUBRESOURCE -> IO HRESULT |], "Map")
		, ([t| Ptr $(forwardRef "ID3D11Resource") -> UINT -> IO () |], "Unmap")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11Buffer")) -> IO () |], "PSSetConstantBuffers")
		, ([t| Ptr $(forwardRef "ID3D11InputLayout") -> IO () |], "IASetInputLayout")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11Buffer")) -> Ptr UINT -> Ptr UINT -> IO () |], "IASetVertexBuffers")
		, ([t| Ptr $(forwardRef "ID3D11Buffer") -> EnumWrapper DXGI_FORMAT -> UINT -> IO () |], "IASetIndexBuffer")
		, ([t| UINT -> UINT -> UINT -> INT -> UINT -> IO () |], "DrawIndexedInstanced")
		, ([t| UINT -> UINT -> UINT -> UINT -> IO () |], "DrawInstanced")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11Buffer")) -> IO () |], "GSSetConstantBuffers")
		, ([t| Ptr $(forwardRef "ID3D11GeometryShader") -> Ptr (Ptr $(forwardRef "ID3D11ClassInstance")) -> UINT -> IO () |], "GSSetShader")
		, ([t| EnumWrapper D3D11_PRIMITIVE_TOPOLOGY -> IO () |], "IASetPrimitiveTopology")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11ShaderResourceView")) -> IO () |], "VSSetShaderResources")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11SamplerState")) -> IO () |], "VSSetSamplers")
		, ([t| Ptr $(forwardRef "ID3D11Asynchronous") -> IO () |], "Begin")
		, ([t| Ptr $(forwardRef "ID3D11Asynchronous") -> IO () |], "End")
		, ([t| Ptr $(forwardRef "ID3D11Asynchronous") -> Ptr () -> UINT -> UINT -> IO HRESULT |], "GetData")
		, ([t| Ptr $(forwardRef "ID3D11Predicate") -> BOOL -> IO () |], "SetPredication")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11ShaderResourceView")) -> IO () |], "GSSetShaderResources")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11SamplerState")) -> IO () |], "GSSetSamplers")
		, ([t| UINT -> Ptr (Ptr $(forwardRef "ID3D11RenderTargetView")) -> Ptr $(forwardRef "ID3D11DepthStencilView") -> IO () |], "OMSetRenderTargets")
		, ([t| UINT -> Ptr (Ptr $(forwardRef "ID3D11RenderTargetView")) -> Ptr $(forwardRef "ID3D11DepthStencilView") -> UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11UnorderedAccessView")) -> Ptr UINT -> IO () |], "OMSetRenderTargetsAndUnorderedAccessViews")
		, ([t| Ptr $(forwardRef "ID3D11BlendState") -> Ptr FLOAT -> UINT -> IO () |], "OMSetBlendState")
		, ([t| Ptr $(forwardRef "ID3D11DepthStencilState") -> UINT -> IO () |], "OMSetDepthStencilState")
		, ([t| UINT -> Ptr (Ptr $(forwardRef "ID3D11Buffer")) -> Ptr UINT -> IO () |], "SOSetTargets")
		, ([t| IO () |], "DrawAuto")
		, ([t| Ptr $(forwardRef "ID3D11Buffer") -> UINT -> IO () |], "DrawIndexedInstancedIndirect")
		, ([t| Ptr $(forwardRef "ID3D11Buffer") -> UINT -> IO () |], "DrawInstancedIndirect")
		, ([t| UINT -> UINT -> UINT -> IO () |], "Dispatch")
		, ([t| Ptr $(forwardRef "ID3D11Buffer") -> UINT -> IO () |], "DispatchIndirect")
		, ([t| Ptr $(forwardRef "ID3D11RasterizerState") -> IO () |], "RSSetState")
		, ([t| UINT -> Ptr D3D11_VIEWPORT -> IO () |], "RSSetViewports")
		, ([t| UINT -> Ptr D3D11_RECT -> IO () |], "RSSetScissorRects")
		, ([t| Ptr $(forwardRef "ID3D11Resource") -> UINT -> UINT -> UINT -> UINT -> Ptr $(forwardRef "ID3D11Resource") -> UINT -> Ptr D3D11_BOX -> IO () |], "CopySubresourceRegion")
		, ([t| Ptr $(forwardRef "ID3D11Resource") -> Ptr $(forwardRef "ID3D11Resource") -> IO () |], "CopyResource")
		, ([t| Ptr $(forwardRef "ID3D11Resource") -> UINT -> Ptr D3D11_BOX -> Ptr () -> UINT -> UINT -> IO () |], "UpdateSubresource")
		, ([t| Ptr $(forwardRef "ID3D11Buffer") -> UINT -> Ptr $(forwardRef "ID3D11UnorderedAccessView") -> IO () |], "CopyStructureCount")
		, ([t| Ptr $(forwardRef "ID3D11RenderTargetView") -> Ptr FLOAT -> IO () |], "ClearRenderTargetView")
		, ([t| Ptr $(forwardRef "ID3D11UnorderedAccessView") -> Ptr UINT -> IO () |], "ClearUnorderedAccessViewUint")
		, ([t| Ptr $(forwardRef "ID3D11UnorderedAccessView") -> Ptr FLOAT -> IO () |], "ClearUnorderedAccessViewFloat")
		, ([t| Ptr $(forwardRef "ID3D11DepthStencilView") -> UINT -> FLOAT -> UINT8 -> IO () |], "ClearDepthStencilView")
		, ([t| Ptr $(forwardRef "ID3D11ShaderResourceView") -> IO () |], "GenerateMips")
		, ([t| Ptr $(forwardRef "ID3D11Resource") -> FLOAT -> IO () |], "SetResourceMinLOD")
		, ([t| Ptr $(forwardRef "ID3D11Resource") -> IO FLOAT |], "GetResourceMinLOD")
		, ([t| Ptr $(forwardRef "ID3D11Resource") -> UINT -> Ptr $(forwardRef "ID3D11Resource") -> UINT -> EnumWrapper DXGI_FORMAT -> IO () |], "ResolveSubresource")
		, ([t| Ptr $(forwardRef "ID3D11CommandList") -> BOOL -> IO () |], "ExecuteCommandList")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11ShaderResourceView")) -> IO () |], "HSSetShaderResources")
		, ([t| Ptr $(forwardRef "ID3D11HullShader") -> Ptr (Ptr $(forwardRef "ID3D11ClassInstance")) -> UINT -> IO () |], "HSSetShader")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11SamplerState")) -> IO () |], "HSSetSamplers")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11Buffer")) -> IO () |], "HSSetConstantBuffers")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11ShaderResourceView")) -> IO () |], "DSSetShaderResources")
		, ([t| Ptr $(forwardRef "ID3D11DomainShader") -> Ptr (Ptr $(forwardRef "ID3D11ClassInstance")) -> UINT -> IO () |], "DSSetShader")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11SamplerState")) -> IO () |], "DSSetSamplers")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11Buffer")) -> IO () |], "DSSetConstantBuffers")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11ShaderResourceView")) -> IO () |], "CSSetShaderResources")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11UnorderedAccessView")) -> Ptr UINT -> IO () |], "CSSetUnorderedAccessViews")
		, ([t| Ptr $(forwardRef "ID3D11ComputeShader") -> Ptr (Ptr $(forwardRef "ID3D11ClassInstance")) -> UINT -> IO () |], "CSSetShader")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11SamplerState")) -> IO () |], "CSSetSamplers")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11Buffer")) -> IO () |], "CSSetConstantBuffers")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11Buffer")) -> IO () |], "VSGetConstantBuffers")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11ShaderResourceView")) -> IO () |], "PSGetShaderResources")
		, ([t| Ptr (Ptr $(forwardRef "ID3D11PixelShader")) -> Ptr (Ptr $(forwardRef "ID3D11ClassInstance")) -> Ptr UINT -> IO () |], "PSGetShader")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11SamplerState")) -> IO () |], "PSGetSamplers")
		, ([t| Ptr (Ptr $(forwardRef "ID3D11VertexShader")) -> Ptr (Ptr $(forwardRef "ID3D11ClassInstance")) -> Ptr UINT -> IO () |], "VSGetShader")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11Buffer")) -> IO () |], "PSGetConstantBuffers")
		, ([t| Ptr (Ptr $(forwardRef "ID3D11InputLayout")) -> IO () |], "IAGetInputLayout")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11Buffer")) -> Ptr UINT -> Ptr UINT -> IO () |], "IAGetVertexBuffers")
		, ([t| Ptr (Ptr $(forwardRef "ID3D11Buffer")) -> Ptr DXGI_FORMAT -> Ptr UINT -> IO () |], "IAGetIndexBuffer")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11Buffer")) -> IO () |], "GSGetConstantBuffers")
		, ([t| Ptr (Ptr $(forwardRef "ID3D11GeometryShader")) -> Ptr (Ptr $(forwardRef "ID3D11ClassInstance")) -> Ptr UINT -> IO () |], "GSGetShader")
		, ([t| Ptr D3D11_PRIMITIVE_TOPOLOGY -> IO () |], "IAGetPrimitiveTopology")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11ShaderResourceView")) -> IO () |], "VSGetShaderResources")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11SamplerState")) -> IO () |], "VSGetSamplers")
		, ([t| Ptr (Ptr $(forwardRef "ID3D11Predicate")) -> Ptr BOOL -> IO () |], "GetPredication")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11ShaderResourceView")) -> IO () |], "GSGetShaderResources")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11SamplerState")) -> IO () |], "GSGetSamplers")
		, ([t| UINT -> Ptr (Ptr $(forwardRef "ID3D11RenderTargetView")) -> Ptr (Ptr $(forwardRef "ID3D11DepthStencilView")) -> IO () |], "OMGetRenderTargets")
		, ([t| UINT -> Ptr (Ptr $(forwardRef "ID3D11RenderTargetView")) -> Ptr (Ptr $(forwardRef "ID3D11DepthStencilView")) -> UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11UnorderedAccessView")) -> IO () |], "OMGetRenderTargetsAndUnorderedAccessViews")
		, ([t| Ptr $(forwardRef "ID3D11BlendState") -> Ptr FLOAT -> Ptr UINT -> IO () |], "OMGetBlendState")
		, ([t| Ptr (Ptr $(forwardRef "ID3D11DepthStencilState")) -> Ptr UINT -> IO () |], "OMGetDepthStencilState")
		, ([t| UINT -> Ptr (Ptr $(forwardRef "ID3D11Buffer")) -> IO () |], "SOGetTargets")
		, ([t| Ptr (Ptr $(forwardRef "ID3D11RasterizerState")) -> IO () |], "RSGetState")
		, ([t| Ptr UINT -> Ptr D3D11_VIEWPORT -> IO () |], "RSGetViewports")
		, ([t| Ptr UINT -> Ptr D3D11_RECT -> IO () |], "RSGetScissorRects")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11ShaderResourceView")) -> IO () |], "HSGetShaderResources")
		, ([t| Ptr (Ptr $(forwardRef "ID3D11HullShader")) -> Ptr (Ptr $(forwardRef "ID3D11ClassInstance")) -> Ptr UINT -> IO () |], "HSGetShader")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11SamplerState")) -> IO () |], "HSGetSamplers")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11Buffer")) -> IO () |], "HSGetConstantBuffers")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11ShaderResourceView")) -> IO () |], "DSGetShaderResources")
		, ([t| Ptr (Ptr $(forwardRef "ID3D11DomainShader")) -> Ptr (Ptr $(forwardRef "ID3D11ClassInstance")) -> Ptr UINT -> IO () |], "DSGetShader")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11SamplerState")) -> IO () |], "DSGetSamplers")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11Buffer")) -> IO () |], "DSGetConstantBuffers")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11ShaderResourceView")) -> IO () |], "CSGetShaderResources")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11UnorderedAccessView")) -> IO () |], "CSGetUnorderedAccessViews")
		, ([t| Ptr (Ptr $(forwardRef "ID3D11ComputeShader")) -> Ptr (Ptr $(forwardRef "ID3D11ClassInstance")) -> Ptr UINT -> IO () |], "CSGetShader")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11SamplerState")) -> IO () |], "CSGetSamplers")
		, ([t| UINT -> UINT -> Ptr (Ptr $(forwardRef "ID3D11Buffer")) -> IO () |], "CSGetConstantBuffers")
		, ([t| IO () |], "ClearState")
		, ([t| IO () |], "Flush")
		, ([t| IO (EnumWrapper D3D11_DEVICE_CONTEXT_TYPE) |], "GetType")
		, ([t| IO UINT |], "GetContextFlags")
		, ([t| BOOL -> Ptr (Ptr $(forwardRef "ID3D11CommandList")) -> IO HRESULT |], "FinishCommandList")
		]

	-- ID3D11Device
	, genCOMInterface "ID3D11Device" "db6f6ddb-ac77-4e88-8253-819df9bbf140" ["IUnknown"]
		[ ([t| Ptr D3D11_BUFFER_DESC -> Ptr D3D11_SUBRESOURCE_DATA -> Ptr (Ptr $(forwardRef "ID3D11Buffer")) -> IO HRESULT |], "CreateBuffer")
		, ([t| Ptr D3D11_TEXTURE1D_DESC -> Ptr D3D11_SUBRESOURCE_DATA -> Ptr (Ptr $(forwardRef "ID3D11Texture1D")) -> IO HRESULT |], "CreateTexture1D")
		, ([t| Ptr D3D11_TEXTURE2D_DESC -> Ptr D3D11_SUBRESOURCE_DATA -> Ptr (Ptr $(forwardRef "ID3D11Texture2D")) -> IO HRESULT |], "CreateTexture2D")
		, ([t| Ptr D3D11_TEXTURE3D_DESC -> Ptr D3D11_SUBRESOURCE_DATA -> Ptr (Ptr $(forwardRef "ID3D11Texture3D")) -> IO HRESULT |], "CreateTexture3D")
		, ([t| Ptr $(forwardRef "ID3D11Resource") -> Ptr D3D11_SHADER_RESOURCE_VIEW_DESC -> Ptr (Ptr $(forwardRef "ID3D11ShaderResourceView")) -> IO HRESULT |], "CreateShaderResourceView")
		, ([t| Ptr $(forwardRef "ID3D11Resource") -> Ptr D3D11_UNORDERED_ACCESS_VIEW_DESC -> Ptr (Ptr $(forwardRef "ID3D11UnorderedAccessView")) -> IO HRESULT |], "CreateUnorderedAccessView")
		, ([t| Ptr $(forwardRef "ID3D11Resource") -> Ptr D3D11_RENDER_TARGET_VIEW_DESC -> Ptr (Ptr $(forwardRef "ID3D11RenderTargetView")) -> IO HRESULT |], "CreateRenderTargetView")
		, ([t| Ptr $(forwardRef "ID3D11Resource") -> Ptr D3D11_DEPTH_STENCIL_VIEW_DESC -> Ptr (Ptr $(forwardRef "ID3D11DepthStencilView")) -> IO HRESULT |], "CreateDepthStencilView")
		, ([t| Ptr D3D11_INPUT_ELEMENT_DESC -> UINT -> Ptr () -> SIZE_T -> Ptr (Ptr $(forwardRef "ID3D11InputLayout")) -> IO HRESULT |], "CreateInputLayout")
		, ([t| Ptr () -> SIZE_T -> Ptr $(forwardRef "ID3D11ClassLinkage") -> Ptr (Ptr $(forwardRef "ID3D11VertexShader")) -> IO HRESULT |], "CreateVertexShader")
		, ([t| Ptr () -> SIZE_T -> Ptr $(forwardRef "ID3D11ClassLinkage") -> Ptr (Ptr $(forwardRef "ID3D11GeometryShader")) -> IO HRESULT |], "CreateGeometryShader")
		, ([t| Ptr () -> SIZE_T -> Ptr D3D11_SO_DECLARATION_ENTRY -> UINT -> Ptr UINT -> UINT -> UINT -> Ptr $(forwardRef "ID3D11ClassLinkage") -> Ptr (Ptr $(forwardRef "ID3D11GeometryShader")) -> IO HRESULT |], "CreateGeometryShaderWithStreamOutput")
		, ([t| Ptr () -> SIZE_T -> Ptr $(forwardRef "ID3D11ClassLinkage") -> Ptr (Ptr $(forwardRef "ID3D11PixelShader")) -> IO HRESULT |], "CreatePixelShader")
		, ([t| Ptr () -> SIZE_T -> Ptr $(forwardRef "ID3D11ClassLinkage") -> Ptr (Ptr $(forwardRef "ID3D11HullShader")) -> IO HRESULT |], "CreateHullShader")
		, ([t| Ptr () -> SIZE_T -> Ptr $(forwardRef "ID3D11ClassLinkage") -> Ptr (Ptr $(forwardRef "ID3D11DomainShader")) -> IO HRESULT |], "CreateDomainShader")
		, ([t| Ptr () -> SIZE_T -> Ptr $(forwardRef "ID3D11ClassLinkage") -> Ptr (Ptr $(forwardRef "ID3D11ComputeShader")) -> IO HRESULT |], "CreateComputeShader")
		, ([t| Ptr $(forwardRef "ID3D11ClassLinkage") -> IO HRESULT |], "CreateClassLinkage")
		, ([t| Ptr D3D11_BLEND_DESC -> Ptr (Ptr $(forwardRef "ID3D11BlendState")) -> IO HRESULT |], "CreateBlendState")
		, ([t| Ptr D3D11_DEPTH_STENCIL_DESC -> Ptr (Ptr $(forwardRef "ID3D11DepthStencilState")) -> IO HRESULT |], "CreateDepthStencilState")
		, ([t| Ptr D3D11_RASTERIZER_DESC -> Ptr (Ptr $(forwardRef "ID3D11RasterizerState")) -> IO HRESULT |], "CreateRasterizerState")
		, ([t| Ptr D3D11_SAMPLER_DESC -> Ptr (Ptr $(forwardRef "ID3D11SamplerState")) -> IO HRESULT |], "CreateSamplerState")
		, ([t| Ptr D3D11_QUERY_DESC -> Ptr (Ptr $(forwardRef "ID3D11Query")) -> IO HRESULT |], "CreateQuery")
		, ([t| Ptr D3D11_QUERY_DESC -> Ptr (Ptr $(forwardRef "ID3D11Predicate")) -> IO HRESULT |], "CreatePredicate")
		, ([t| Ptr D3D11_COUNTER_DESC -> Ptr (Ptr $(forwardRef "ID3D11Counter")) -> IO HRESULT |], "CreateCounter")
		, ([t| UINT -> Ptr (Ptr $(forwardRef "ID3D11DeviceContext")) -> IO HRESULT |], "CreateDeferredContext")
		, ([t| HANDLE -> REFIID -> Ptr (Ptr ()) -> IO HRESULT |], "OpenSharedResource")
		, ([t| EnumWrapper DXGI_FORMAT -> Ptr UINT -> IO HRESULT |], "CheckFormatSupport")
		, ([t| EnumWrapper DXGI_FORMAT -> UINT -> Ptr UINT -> IO HRESULT |], "CheckMultisampleQualityLevels")
		, ([t| Ptr D3D11_COUNTER_INFO -> IO () |], "CheckCounterInfo")
		, ([t| Ptr D3D11_COUNTER_DESC -> Ptr D3D11_COUNTER_TYPE -> Ptr UINT -> LPSTR -> Ptr UINT -> LPSTR -> Ptr UINT -> LPSTR -> Ptr UINT -> IO HRESULT |], "CheckCounter")
		, ([t| EnumWrapper D3D11_FEATURE -> Ptr () -> UINT -> IO HRESULT |], "CheckFeatureSupport")
		, ([t| REFGUID -> Ptr UINT -> Ptr () -> IO HRESULT |], "GetPrivateData")
		, ([t| REFGUID -> UINT -> UINT -> Ptr () -> IO HRESULT |], "SetPrivateData")
		, ([t| REFGUID -> Ptr IUnknown -> IO HRESULT |], "SetPrivateDataInterface")
		, ([t| IO (EnumWrapper D3D_FEATURE_LEVEL) |], "GetFeatureLevel")
		, ([t| IO UINT |], "GetCreationFlags")
		, ([t| IO HRESULT |], "GetDeviceRemovedReason")
		, ([t| Ptr (Ptr $(forwardRef "ID3D11DeviceContext")) -> IO () |], "GetImmediateContext")
		, ([t| UINT -> IO HRESULT |], "SetExceptionMode")
		, ([t| IO UINT |], "GetExceptionMode")
		]
	]

-- | Wrapper for D3D11CreateDevice
d3d11CreateDevice :: D3D11CreateDeviceProc
d3d11CreateDevice a b c d e f g h i j = do
	proc <- loadLibraryAndGetProcAddress "d3d11.dll" "D3D11CreateDevice"
	mkD3D11CreateDeviceProc proc a b c d e f g h i j

type D3D11CreateDeviceProc
	=  Ptr IDXGIAdapter
	-> EnumWrapper D3D_DRIVER_TYPE
	-> HMODULE
	-> UINT
	-> Ptr D3D_FEATURE_LEVEL
	-> UINT
	-> UINT
	-> Ptr (Ptr ID3D11Device)
	-> Ptr D3D_FEATURE_LEVEL
	-> Ptr (Ptr ID3D11DeviceContext)
	-> IO HRESULT

foreign import stdcall safe "dynamic" mkD3D11CreateDeviceProc :: FunPtr D3D11CreateDeviceProc -> D3D11CreateDeviceProc

-- | SDK Version from Windows 8 SDK.
d3d11SdkVersion :: UINT
d3d11SdkVersion = 7

type D3DCompileProc
	=  Ptr () -- pSrcData
	-> SIZE_T -- SrcDataSize
	-> LPSTR -- pSourceName
	-> Ptr () -- pDefines
	-> Ptr () -- pInclude
	-> LPSTR -- pEntrypoint
	-> LPSTR -- pTarget
	-> UINT -- Flags1
	-> UINT -- Flags2
	-> Ptr (Ptr ID3DBlob) -- ppCode
	-> Ptr (Ptr ID3DBlob) -- ppErrorMsgs
	-> IO HRESULT

-- | D3DCompile.
foreign import stdcall safe "dynamic" mkD3DCompile :: FunPtr D3DCompileProc -> D3DCompileProc
