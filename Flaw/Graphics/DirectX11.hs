{-|
Module: Flaw.Graphics.DirectX11
Description: DirectX 11 implementation of graphics.
License: MIT
-}

{-# LANGUAGE TemplateHaskell #-}

module Flaw.Graphics.DirectX11
	( D3D_DRIVER_TYPE
	, D3D_FEATURE_LEVEL
	, d3d11CreateDevice
	) where

import Data.Word
import Foreign.Ptr

import Flaw.FFI
import Flaw.FFI.COM
import Flaw.FFI.COM.TH
import Flaw.FFI.Win32
import Flaw.Graphics.Internal
import Flaw.Graphics.DXGI

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

-- | D3D11_BUFFER_DESC
genStruct "D3D11_BUFFER_DESC"
	[ ([t|UINT|], "ByteWidth", 0)
	, ([t|D3D11_USAGE|], "Usage", 0)
	, ([t|UINT|], "BindFlags", 0)
	, ([t|UINT|], "CPUAccessFlags", 0)
	, ([t|UINT|], "MiscFlags", 0)
	, ([t|UINT|], "StructureByteStride", 0)
	]

-- | D3D11_SUBRESOURCE_DATA
genStruct "D3D11_SUBRESOURCE_DATA"
	[ ([t|Ptr ()|], "pSysMem", 0)
	, ([t|UINT|], "SysMemPitch", 0)
	, ([t|UINT|], "SysMemSlicePitch", 0)
	]

-- | D3D11_TEXTURE1D_DESC
genStruct "D3D11_TEXTURE1D_DESC"
	[ ([t|UINT|], "Width", 0)
	, ([t|UINT|], "MipLevels", 0)
	, ([t|UINT|], "ArraySize", 0)
	, ([t|DXGI_FORMAT|], "Format", 0)
	, ([t|D3D11_USAGE|], "Usage", 0)
	, ([t|UINT|], "BindFlags", 0)
	, ([t|UINT|], "CPUAccessFlags", 0)
	, ([t|UINT|], "MiscFlags", 0)
	]

-- | D3D11_TEXTURE2D_DESC
genStruct "D3D11_TEXTURE2D_DESC"
	[ ([t|UINT|], "Width", 0)
	, ([t|UINT|], "Height", 0)
	, ([t|UINT|], "MipLevels", 0)
	, ([t|UINT|], "ArraySize", 0)
	, ([t|DXGI_FORMAT|], "Format", 0)
	, ([t|DXGI_SAMPLE_DESC|], "SampleDesc", 0)
	, ([t|D3D11_USAGE|], "Usage", 0)
	, ([t|UINT|], "BindFlags", 0)
	, ([t|UINT|], "CPUAccessFlags", 0)
	, ([t|UINT|], "MiscFlags", 0)
	]

-- | D3D11_TEXTURE3D_DESC
genStruct "D3D11_TEXTURE3D_DESC"
	[ ([t|UINT|], "Width", 0)
	, ([t|UINT|], "Height", 0)
	, ([t|UINT|], "Depth", 0)
	, ([t|UINT|], "MipLevels", 0)
	, ([t|DXGI_FORMAT|], "Format", 0)
	, ([t|D3D11_USAGE|], "Usage", 0)
	, ([t|UINT|], "BindFlags", 0)
	, ([t|UINT|], "CPUAccessFlags", 0)
	, ([t|UINT|], "MiscFlags", 0)
	]

-- | D3D11_BUFFER_SRV
genStruct "D3D11_BUFFER_SRV"
	[ ([t|UINT|], "FirstElementOrElementOffset", 0)
	, ([t|UINT|], "NumElementsOrElementWidth", 0)
	]

-- | D3D11_TEX1D_SRV
genStruct "D3D11_TEX1D_SRV"
	[ ([t|UINT|], "MostDetailedMip", 0)
	, ([t|UINT|], "MipLevels", 0)
	]

-- | D3D11_TEX1D_ARRAY_SRV
genStruct "D3D11_TEX1D_ARRAY_SRV"
	[ ([t|UINT|], "MostDetailedMip", 0)
	, ([t|UINT|], "MipLevels", 0)
	, ([t|UINT|], "FirstArraySlice", 0)
	, ([t|UINT|], "ArraySize", 0)
	]

-- | D3D11_TEX2D_SRV
genStruct "D3D11_TEX2D_SRV"
	[ ([t|UINT|], "MostDetailedMip", 0)
	, ([t|UINT|], "MipLevels", 0)
	]

-- | D3D11_TEX2D_ARRAY_SRV
genStruct "D3D11_TEX2D_ARRAY_SRV"
	[ ([t|UINT|], "MostDetailedMip", 0)
	, ([t|UINT|], "MipLevels", 0)
	, ([t|UINT|], "FirstArraySlice", 0)
	, ([t|UINT|], "ArraySize", 0)
	]

-- | D3D11_TEX2DMS_SRV
genStruct "D3D11_TEX2DMS_SRV"
	[ ([t|UINT|], "UnusedField_NothingToDefine", 0)
	]

-- | D3D11_TEX2DMS_ARRAY_SRV
genStruct "D3D11_TEX2DMS_ARRAY_SRV"
	[ ([t|UINT|], "FirstArraySlice", 0)
	, ([t|UINT|], "ArraySize", 0)
	]

-- | D3D11_TEX3D_SRV
genStruct "D3D11_TEX3D_SRV"
	[ ([t|UINT|], "MostDetailedMip", 0)
	, ([t|UINT|], "MipLevels", 0)
	]

-- | D3D11_TEXCUBE_SRV
genStruct "D3D11_TEXCUBE_SRV"
	[ ([t|UINT|], "MostDetailedMip", 0)
	, ([t|UINT|], "MipLevels", 0)
	]

-- | D3D11_TEXCUBE_ARRAY_SRV
genStruct "D3D11_TEXCUBE_ARRAY_SRV"
	[ ([t|UINT|], "MostDetailedMip", 0)
	, ([t|UINT|], "MipLevels", 0)
	, ([t|UINT|], "First2DArrayFace", 0)
	, ([t|UINT|], "NumCubes", 0)
	]

-- | D3D11_BUFFEREX_SRV
genStruct "D3D11_BUFFEREX_SRV"
	[ ([t|UINT|], "FirstElement", 0)
	, ([t|UINT|], "NumElements", 0)
	, ([t|UINT|], "Flags", 0)
	]

-- | D3D11_SHADER_RESOURCE_VIEW_DESC
genStructWithEndUnion "D3D11_SHADER_RESOURCE_VIEW_DESC"
	[ ([t|DXGI_FORMAT|], "Format", 0)
	, ([t|D3D11_SRV_DIMENSION|], "ViewDimension", 0)
	] 1
	[ ("D3D11_SRV_DIMENSION_UNKNOWN", [t|()|], "Unknown")
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

-- | D3D11_SHADER_RESOURCE_VIEW_DESC
-- | ID3D11ShaderResourceView
-- | D3D11_UNORDERED_ACCESS_VIEW_DESC
-- | ID3D11UnorderedAccessView
-- | D3D11_RENDER_TARGET_VIEW_DESC
-- | ID3D11RenderTargetView
-- | D3D11_DEPTH_STENCIL_VIEW_DESC
-- | ID3D11DepthStencilView
-- | D3D11_INPUT_ELEMENT_DESC
-- | ID3D11InputLayout
-- | ID3D11ClassLinkage
-- | ID3D11VertexShader
-- | ID3D11GeometryShader
-- | D3D11_SO_DECLARATION_ENTRY
-- | ID3D11PixelShader
-- | ID3D11HullShader
-- | ID3D11DomainShader
-- | CreateComputeShader
-- | D3D11_BLEND_DESC
-- | ID3D11BlendState
-- | D3D11_DEPTH_STENCIL_DESC
-- | ID3D11DepthStencilState
-- | D3D11_RASTERIZER_DESC
-- | ID3D11RasterizerState
-- | D3D11_SAMPLER_DESC
-- | ID3D11SamplerState
-- | D3D11_QUERY_DESC
-- | ID3D11Query
-- | D3D11_QUERY_DESC
-- | ID3D11Predicate
-- | D3D11_COUNTER_DESC
-- | ID3D11Counter
-- | D3D11_COUNTER_INFO
-- | D3D11_COUNTER_TYPE

-- | ID3D11Resource
genCOMInterface "ID3D11Resource" "dc8e63f3-d12b-4952-b47b-5e45026a862d" (Just "ID3D11DeviceChild")
	[ ([t| Ptr D3D11_RESOURCE_DIMENSION -> IO () |], "GetType")
	, ([t| UINT -> IO () |], "SetEvictionPriority")
	, ([t| IO UINT |], "GetEvictionPriority")
	]

-- | ID3D11DeviceContext
genCOMInterface "ID3D11DeviceContext" "c0bfa96c-e089-44fb-8eaf-26f8796190da" (Just "ID3D11DeviceChild")
	[ ([t| UINT -> UINT -> Ptr (Ptr ID3D11Buffer) -> IO () |], "VSSetConstantBuffers")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11ShaderResourceView) -> IO () |], "PSSetShaderResources")
	, ([t| Ptr ID3D11PixelShader -> Ptr (Ptr ID3D11ClassInstance) -> UINT -> IO () |], "PSSetShader")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11SamplerState) -> IO () |], "PSSetSamplers")
	, ([t| Ptr ID3D11VertexShader -> Ptr (Ptr ID3D11ClassInstance) -> UINT -> IO () |], "VSSetShader")
	, ([t| UINT -> UINT -> INT -> IO () |], "DrawIndexed")
	, ([t| UINT -> UINT -> IO () |], "Draw")
	, ([t| Ptr ID3D11Resource -> UINT -> D3D11_MAP -> UINT -> Ptr D3D11_MAPPED_SUBRESOURCE -> IO HRESULT |], "Map")
	, ([t| Ptr ID3D11Resource -> UINT -> IO () |], "Unmap")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11Buffer) -> IO () |], "PSSetConstantBuffers")
	, ([t| Ptr ID3D11InputLayout -> IO () |], "IASetInputLayout")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11Buffer) -> Ptr UINT -> Ptr UINT -> IO () |], "IASetVertexBuffers")
	, ([t| Ptr ID3D11Buffer -> DXGI_FORMAT -> UINT -> IO () |], "IASetIndexBuffer")
	, ([t| UINT -> UINT -> UINT -> INT -> UINT -> IO () |], "DrawIndexedInstanced")
	, ([t| UINT -> UINT -> UINT -> UINT -> IO () |], "DrawInstanced")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11Buffer) -> IO () |], "GSSetConstantBuffers")
	, ([t| Ptr ID3D11GeometryShader -> Ptr (Ptr ID3D11ClassInstance) -> UINT -> IO () |], "GSSetShader")
	, ([t| D3D11_PRIMITIVE_TOPOLOGY -> IO () |], "IASetPrimitiveTopology")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11ShaderResourceView) -> IO () |], "VSSetShaderResources")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11SamplerState) -> IO () |], "VSSetSamplers")
	, ([t| Ptr ID3D11Asynchronous -> IO () |], "Begin")
	, ([t| Ptr ID3D11Asynchronous -> IO () |], "End")
	, ([t| Ptr ID3D11Asynchronous -> Ptr () -> UINT -> UINT -> IO HRESULT |], "GetData")
	, ([t| Ptr ID3D11Predicate -> BOOL -> IO () |], "SetPredication")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11ShaderResourceView) -> IO () |], "GSSetShaderResources")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11SamplerState) -> IO () |], "GSSetSamplers")
	, ([t| UINT -> Ptr (Ptr ID3D11RenderTargetView) -> Ptr ID3D11DepthStencilView -> IO () |], "OMSetRenderTargets")
	, ([t| UINT -> Ptr (Ptr ID3D11RenderTargetView) -> Ptr ID3D11DepthStencilView -> UINT -> UINT -> Ptr (Ptr ID3D11UnorderedAccessView) -> Ptr UINT -> IO () |], "OMSetRenderTargetsAndUnorderedAccessViews")
	, ([t| Ptr ID3D11BlendState -> Ptr FLOAT -> UINT -> IO () |], "OMSetBlendState")
	, ([t| Ptr ID3D11DepthStencilState -> UINT -> IO () |], "OMSetDepthStencilState")
	, ([t| UINT -> Ptr (Ptr ID3D11Buffer) -> Ptr UINT -> IO () |], "SOSetTargets")
	, ([t| IO () |], "DrawAuto")
	, ([t| Ptr ID3D11Buffer -> UINT -> IO () |], "DrawIndexedInstancedIndirect")
	, ([t| Ptr ID3D11Buffer -> UINT -> IO () |], "DrawInstancedIndirect")
	, ([t| UINT -> UINT -> UINT -> IO () |], "Dispatch")
	, ([t| Ptr ID3D11Buffer -> UINT -> IO () |], "DispatchIndirect")
	, ([t| Ptr ID3D11RasterizerState -> IO () |], "RSSetState")
	, ([t| UINT -> Ptr D3D11_VIEWPORT -> IO () |], "RSSetViewports")
	, ([t| UINT -> Ptr D3D11_RECT -> IO () |], "RSSetScissorRects")
	, ([t| Ptr ID3D11Resource -> UINT -> UINT -> UINT -> UINT -> Ptr ID3D11Resource -> UINT -> Ptr D3D11_BOX -> IO () |], "CopySubresourceRegion")
	, ([t| Ptr ID3D11Resource -> Ptr ID3D11Resource -> IO () |], "CopyResource")
	, ([t| Ptr ID3D11Resource -> UINT -> Ptr D3D11_BOX -> Ptr () -> UINT -> UINT -> IO () |], "UpdateSubresource")
	, ([t| Ptr ID3D11Buffer -> UINT -> Ptr ID3D11UnorderedAccessView -> IO () |], "CopyStructureCount")
	, ([t| Ptr ID3D11RenderTargetView -> Ptr FLOAT -> IO () |], "ClearRenderTargetView")
	, ([t| Ptr ID3D11UnorderedAccessView -> Ptr UINT -> IO () |], "ClearUnorderedAccessViewUint")
	, ([t| Ptr ID3D11UnorderedAccessView -> Ptr FLOAT -> IO () |], "ClearUnorderedAccessViewFloat")
	, ([t| Ptr ID3D11DepthStencilView -> UINT -> FLOAT -> UINT8 -> IO () |], "CreateDepthStencilView")
	, ([t| Ptr ID3D11ShaderResourceView -> IO () |], "GenerateMips")
	, ([t| Ptr ID3D11Resource -> FLOAT -> IO () |], "SetResourceMinLOD")
	, ([t| Ptr ID3D11Resource -> IO FLOAT |], "GetResourceMinLOD")
	, ([t| Ptr ID3D11Resource -> UINT -> Ptr ID3D11Resource -> UINT -> DXGI_FORMAT -> IO () |], "ResolveSubresource")
	. ([t| Ptr ID3D11CommandList -> BOOL -> IO () |], "ExecuteCommandList")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11ShaderResourceView) -> IO () |], "HSSetShaderResources")
	, ([t| Ptr ID3D11HullShader -> Ptr (Ptr ID3D11ClassInstance) -> UINT -> IO () |], "HSSetShader")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11SamplerState) -> IO () |], "HSSetSamplers")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11Buffer) -> IO () |], "HSSetConstantBuffers")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11ShaderResourceView) -> IO () |], "DSSetShaderResources")
	, ([t| Ptr ID3D11DomainShader -> Ptr (Ptr ID3D11ClassInstance) -> UINT -> IO () |], "DSSetShader")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11SamplerState) -> IO () |], "DSSetSamplers")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11Buffer) -> IO () |], "DSSetConstantBuffers")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11ShaderResourceView) -> IO () |], "CSSetShaderResources")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11UnorderedAccessView) -> Ptr UINT -> IO () |], "CSSetUnorderedAccessViews")
	, ([t| Ptr ID3D11ComputeShader -> Ptr (Ptr ID3D11ClassInstance) -> UINT -> IO () |], "CSSetShader")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11SamplerState) -> IO () |], "CSSetSamplers")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11Buffer) -> IO () |], "CSSetConstantBuffers")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11Buffer) -> IO () |], "VSGetConstantBuffers")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11ShaderResourceView) -> IO () |], "PSGetShaderResources")
	, ([t| Ptr (Ptr ID3D11PixelShader) -> Ptr (Ptr ID3D11ClassInstance) -> Ptr UINT -> IO () |], "PSGetShader")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11SamplerState) -> IO () |], "PSGetSamplers")
	, ([t| Ptr (Ptr ID3D11VertexShader) -> Ptr (Ptr ID3D11ClassInstance) -> Ptr UINT -> IO () |], "VSGetShader")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11Buffer) -> IO () |], "PSGetConstantBuffers")
	, ([t| Ptr (Ptr ID3D11InputLayout) -> IO () |], "IAGetInputLayout")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11Buffer) -> Ptr UINT -> Ptr UINT -> IO () |], "IAGetVertexBuffers")
	, ([t| Ptr (Ptr ID3D11Buffer) -> Ptr DXGI_FORMAT -> Ptr UINT -> IO () |], "IAGetIndexBuffer")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11Buffer) -> IO () |], "GSGetConstantBuffers")
	, ([t| Ptr (Ptr ID3D11GeometryShader) -> Ptr (Ptr ID3D11ClassInstance) -> Ptr UINT -> IO () |], "GSGetShader")
	, ([t| Ptr D3D11_PRIMITIVE_TOPOLOGY -> IO () |], "IAGetPrimitiveTopology")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11ShaderResourceView) -> IO () |], "VSGetShaderResources")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11SamplerState) -> IO () |], "VSGetSamplers")
	, ([t| Ptr (Ptr ID3D11Predicate) -> Ptr BOOL -> IO () |], "GetPredication")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11ShaderResourceView) -> IO () |], "GSGetShaderResources")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11SamplerState) -> IO () |], "GSGetSamplers")
	, ([t| UINT -> Ptr (Ptr ID3D11RenderTargetView) -> Ptr (Ptr ID3D11DepthStencilView) -> IO () |], "OMGetRenderTargets")
	, ([t| UINT -> Ptr (Ptr ID3D11RenderTargetView) -> Ptr (Ptr ID3D11DepthStencilView) -> UINT -> UINT -> Ptr (Ptr ID3D11UnorderedAccessView) -> IO () |], "OMGetRenderTargetsAndUnorderedAccessViews")
	, ([t| Ptr ID3D11BlendState -> Ptr FLOAT -> Ptr UINT -> IO () |], "OMGetBlendState")
	, ([t| Ptr (Ptr ID3D11DepthStencilState) -> Ptr UINT -> IO () |], "OMGetDepthStencilState")
	, ([t| UINT -> Ptr (Ptr ID3D11Buffer) -> IO () |], "SOGetTargets")
	, ([t| Ptr (Ptr ID3D11RasterizerState) -> IO () |], "RSGetState")
	, ([t| Ptr UINT -> Ptr D3D11_VIEWPORT -> IO () |], "RSGetViewports")
	, ([t| Ptr UINT -> Ptr D3D11_RECT -> IO () |], "RSGetScissorRects")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11ShaderResourceView) -> IO () |], "HSGetShaderResources")
	, ([t| Ptr (Ptr ID3D11HullShader) -> Ptr (Ptr ID3D11ClassInstance) -> Ptr UINT -> IO () |], "HSGetShader")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11SamplerState) -> IO () |], "HSGetSamplers")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11Buffer) -> IO () |], "HSGetConstantBuffers")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11ShaderResourceView) -> IO () |], "DSGetShaderResources")
	, ([t| Ptr (Ptr ID3D11DomainShader) -> Ptr (Ptr ID3D11ClassInstance) -> Ptr UINT -> IO () |], "DSGetShader")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11SamplerState) -> IO () |], "DSGetSamplers")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11Buffer) -> IO () |], "DSGetConstantBuffers")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11ShaderResourceView) -> IO () |], "CSGetShaderResources")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11UnorderedAccessView) -> IO () |], "CSGetUnorderedAccessViews")
	, ([t| Ptr (Ptr ID3D11ComputeShader) -> Ptr (Ptr ID3D11ClassInstance) -> Ptr UINT -> IO () |], "CSGetShader")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11SamplerState) -> IO () |], "CSGetSamplers")
	, ([t| UINT -> UINT -> Ptr (Ptr ID3D11Buffer) -> IO () |], "CSGetConstantBuffers")
	, ([t| IO () |], "ClearState")
	, ([t| IO () |], "Flush")
	, ([t| IO D3D11_DEVICE_CONTEXT_TYPE |], "GetType")
	, ([t| IO UINT |], "GetContextFlags")
	, ([t| BOOL -> Ptr (Ptr ID3D11CommandList) -> IO HRESULT |], "FinishCommandList")
	]

-- | ID3D11Device
genCOMInterface "ID3D11Device" "db6f6ddb-ac77-4e88-8253-819df9bbf140" (Just "IUnknown")
	[ ([t| Ptr D3D11_BUFFER_DESC -> Ptr D3D11_SUBRESOURCE_DATA -> Ptr (Ptr ID3D11Buffer) -> IO HRESULT |], "CreateBuffer")
	, ([t| Ptr D3D11_TEXTURE1D_DESC -> Ptr D3D11_SUBRESOURCE_DATA -> Ptr (Ptr ID3D11Texture1D) -> IO HRESULT |], "CreateTexture1D")
	, ([t| Ptr D3D11_TEXTURE2D_DESC -> Ptr D3D11_SUBRESOURCE_DATA -> Ptr (Ptr ID3D11Texture2D) -> IO HRESULT |], "CreateTexture2D")
	, ([t| Ptr D3D11_TEXTURE3D_DESC -> Ptr D3D11_SUBRESOURCE_DATA -> Ptr (Ptr ID3D11Texture3D) -> IO HRESULT |], "CreateTexture3D")
	, ([t| Ptr ID3D11Resource -> Ptr D3D11_SHADER_RESOURCE_VIEW_DESC -> Ptr ID3D11ShaderResourceView -> IO HRESULT |], "CreateShaderResourceView")
	, ([t| Ptr ID3D11Resource -> Ptr D3D11_UNORDERED_ACCESS_VIEW_DESC -> Ptr ID3D11UnorderedAccessView -> IO HRESULT |], "CreateUnorderedAccessView")
	, ([t| Ptr ID3D11Resource -> Ptr D3D11_RENDER_TARGET_VIEW_DESC -> Ptr (Ptr ID3D11RenderTargetView) -> IO HRESULT |], "CreateRenderTargetView")
	, ([t| Ptr ID3D11Resource -> Ptr D3D11_DEPTH_STENCIL_VIEW_DESC -> Ptr (Ptr ID3D11DepthStencilView) -> IO HRESULT |], "CreateDepthStencilView")
	, ([t| Ptr D3D11_INPUT_ELEMENT_DESC -> UINT -> Ptr () -> SIZE_T -> Ptr (Ptr ID3D11InputLayout) -> IO HRESULT |], "CreateInputLayout")
	, ([t| Ptr () -> SIZE_T -> Ptr ID3D11ClassLinkage -> Ptr (Ptr ID3D11VertexShader) -> IO HRESULT |], "CreateVertexShader")
	, ([t| Ptr () -> SIZE_T -> Ptr ID3D11ClassLinkage -> Ptr (Ptr ID3D11GeometryShader) -> IO HRESULT |], "CreateGeometryShader")
	, ([t| Ptr () -> SIZE_T -> Ptr D3D11_SO_DECLARATION_ENTRY -> UINT -> Ptr UINT -> UINT -> UINT -> Ptr ID3D11ClassLinkage -> Ptr (Ptr ID3D11GeometryShader) -> IO HRESULT |], "CreateGeometryShaderWithStreamOutput")
	, ([t| Ptr () -> SIZE_T -> Ptr ID3D11ClassLinkage -> Ptr (Ptr ID3D11PixelShader) -> IO HRESULT |], "CreatePixelShader")
	, ([t| Ptr () -> SIZE_T -> Ptr ID3D11ClassLinkage -> Ptr (Ptr ID3D11HullShader) -> IO HRESULT |], "CreateHullShader")
	, ([t| Ptr () -> SIZE_T -> Ptr ID3D11ClassLinkage -> Ptr (Ptr ID3D11DomainShader) -> IO HRESULT |], "CreateDomainShader")
	, ([t| Ptr () -> SIZE_T -> Ptr ID3D11ClassLinkage -> Ptr (Ptr ID3D11ComputeShader) -> IO HRESULT |], "CreateComputeShader")
	, ([t| Ptr ID3D11ClassLinkage -> IO HRESULT |], "CreateClassLinkage")
	, ([t| Ptr D3D11_BLEND_DESC -> Ptr (Ptr ID3D11BlendState) -> IO HRESULT |], "CreateBlendState")
	, ([t| Ptr D3D11_DEPTH_STENCIL_DESC -> Ptr (Ptr ID3D11DepthStencilState) -> IO HRESULT |], "CreateDepthStencilState")
	, ([t| Ptr D3D11_RASTERIZER_DESC -> Ptr (Ptr ID3D11RasterizerState) -> IO HRESULT |], "CreateRasterizerState")
	, ([t| Ptr D3D11_SAMPLER_DESC -> Ptr (Ptr ID3D11SamplerState) -> IO HRESULT |], "CreateSamplerState")
	, ([t| Ptr D3D11_QUERY_DESC -> Ptr (Ptr ID3D11Query) -> IO HRESULT |], "CreateQuery")
	, ([t| Ptr D3D11_QUERY_DESC -> Ptr (Ptr ID3D11Predicate) -> IO HRESULT |], "CreatePredicate")
	, ([t| Ptr D3D11_COUNTER_DESC -> Ptr (Ptr ID3D11Counter) -> IO HRESULT |], "CreateCounter")
	, ([t| UINT -> Ptr (Ptr ID3D11DeviceContext) -> IO HRESULT |], "CreateDeferredContext")
	, ([t| HANDLE -> REFIID -> Ptr (Ptr ()) -> IO HRESULT |], "OpenSharedResource")
	, ([t| DXGI_FORMAT -> Ptr UINT -> IO HRESULT |], "CheckFormatSupport")
	, ([t| DXGI_FORMAT -> UINT -> Ptr UINT -> IO HRESULT |], "CheckMultisampleQualityLevels")
	, ([t| Ptr D3D11_COUNTER_INFO -> IO () |], "CheckCounterInfo")
	, ([t| Ptr D3D11_COUNTER_DESC -> Ptr D3D11_COUNTER_TYPE -> Ptr UINT -> LPSTR -> Ptr UINT -> LPSTR -> Ptr UINT -> LPSTR -> Ptr UINT -> IO HRESULT |], "CheckCounter")
	, ([t| D3D11_FEATURE -> Ptr () -> UINT -> IO HRESULT |], "CheckFeatureSupport")
	, ([t| REFGUID -> Ptr UINT -> Ptr () -> IO HRESULT |], "GetPrivateData")
	, ([t| REFGUID -> UINT -> UINT -> Ptr () -> IO HRESULT |], "SetPrivateData")
	, ([t| REFGUID -> Ptr IUnknown -> IO HRESULT |], "SetPrivateDataInterface")
	, ([t| IO D3D_FEATURE_LEVEL |], "GetFeatureLevel")
	, ([t| IO UINT |], "GetCreationFlags")
	, ([t| IO HRESULT |], "GetDeviceRemovedReason")
	, ([t| Ptr (Ptr ID3D11DeviceContext) -> IO () |], "GetImmediateContext")
	, ([t| UINT -> IO HRESULT |], "SetExceptionMode")
	, ([t| IO UINT |], "GetExceptionMode")
	]

-- | ID3D11DeviceChild
genCOMInterface "ID3D11DeviceChild" "1841e5c8-16b0-489b-bcc8-44cfb0d5deae" (Just "IUnknown")
	[ ([t| Ptr (Ptr ID3D11Device) -> IO () |], "GetDevice")
	, ([t| REFGUID -> Ptr UINT -> Ptr () -> IO HRESULT |], "GetPrivateData")
	, ([t| REFGUID -> UINT -> Ptr () -> IO HRESULT |], "SetPrivateData")
	, ([t| REFGUID -> Ptr IUnknown -> IO HRESULT |], "SetPrivateDataInterface")
	]

-- | Wrapper for D3D11CreateDevice
d3d11CreateDevice :: D3D11CreateDeviceProc
d3d11CreateDevice a b c d e f g h i j = do
	proc <- loadLibraryAndGetProcAddress "d3d11.dll" "D3D11CreateDevice"
	mkD3D11CreateDeviceProc proc a b c d e f g h i j

type D3D11CreateDeviceProc = Ptr IDXGIAdapter
	-> D3D_DRIVER_TYPE
	-> HMODULE
	-> UINT
	-> Ptr D3D_FEATURE_LEVEL
	-> UINT
	-> UINT
	-> Ptr (Ptr ID3D11Device)
	-> Ptr (Ptr ID3D11DeviceContext)
	-> IO HRESULT

foreign import stdcall safe "dynamic" mkD3D11CreateDeviceProc :: FunPtr D3D11CreateDeviceProc -> D3D11CreateDeviceProc
