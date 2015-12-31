{-|
Module: Flaw.Graphics.DirectX11
Description: Internals of graphics implementation for DirectX 11.
License: MIT
-}

{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module Flaw.Graphics.DirectX11
	( Dx11Device(..)
	, dx11CreateDevice
	, Dx11Context(..)
	, Dx11Presenter(..)
	, dx11CreatePresenter
	) where

import Control.Exception
import Control.Monad
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as B
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array(allocaArray, copyArray, withArray)
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Flaw.Book
import Flaw.Exception
import Flaw.FFI
import Flaw.FFI.Win32
import Flaw.FFI.COM
import Flaw.Graphics
import Flaw.Graphics.Blend
import Flaw.Graphics.DirectX11.FFI
import Flaw.Graphics.DirectX11.HLSL
import Flaw.Graphics.DXGI
import Flaw.Graphics.DXGI.FFI
import Flaw.Graphics.Program.Internal
import Flaw.Graphics.Sampler
import Flaw.Graphics.Texture
import Flaw.Math
import Flaw.Window
import Flaw.Window.Win32

-- | DirectX11 graphics device.
data Dx11Device = Dx11Device
	{
	-- | System.
	  dx11DeviceSystem :: !DXGISystem
	-- | Main device interface.
	, dx11DeviceInterface :: !ID3D11Device
	-- | Device's immediate context.
	, dx11DeviceImmediateContext :: !ID3D11DeviceContext
	-- | D3DCompile function.
	, dx11DeviceD3DCompile :: !D3DCompileProc
	-- | Debug mode.
	, dx11DeviceDebug :: !Bool
	}

instance Device Dx11Device where
	type DeferredContext Dx11Device = Dx11Context
	data TextureId Dx11Device
		= Dx11TextureId ID3D11ShaderResourceView !(SamplerStateId Dx11Device)
		| Dx11NullTextureId
		deriving Eq
	data SamplerStateId Dx11Device
		= Dx11SamplerStateId ID3D11SamplerState
		| Dx11NullSamplerStateId
		deriving Eq
	data BlendStateId Dx11Device
		= Dx11BlendStateId ID3D11BlendState
		| Dx11NullBlendStateId
		deriving Eq
	newtype RenderTargetId Dx11Device
		= Dx11RenderTargetId ID3D11RenderTargetView
		deriving Eq
	data DepthStencilTargetId Dx11Device
		= Dx11DepthStencilTargetId ID3D11DepthStencilView
		| Dx11NullDepthStencilTargetId
		deriving Eq
	data FrameBufferId Dx11Device
		= Dx11FrameBufferId [RenderTargetId Dx11Device] (DepthStencilTargetId Dx11Device)
		deriving Eq
	data VertexBufferId Dx11Device
		= Dx11VertexBufferId ID3D11Buffer Int
		| Dx11NullVertexBufferId
		deriving Eq
	data IndexBufferId Dx11Device
		= Dx11IndexBufferId ID3D11Buffer DXGI_FORMAT D3D11_PRIMITIVE_TOPOLOGY
		| Dx11NullIndexBufferId D3D11_PRIMITIVE_TOPOLOGY
		deriving Eq
	data ProgramId Dx11Device
		= Dx11VertexPixelProgramId ID3D11InputLayout ID3D11VertexShader ID3D11PixelShader
		| Dx11NullProgramId
		deriving Eq
	data UniformBufferId Dx11Device
		= Dx11UniformBufferId ID3D11Buffer
		| Dx11NullUniformBufferId
		deriving Eq

	nullTexture = Dx11NullTextureId
	nullSamplerState = Dx11NullSamplerStateId
	nullBlendState = Dx11NullBlendStateId
	nullDepthStencilTarget = Dx11NullDepthStencilTargetId
	nullIndexBuffer = Dx11NullIndexBufferId D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST
	nullUniformBuffer = Dx11NullUniformBufferId

	createDeferredContext device@Dx11Device
		{ dx11DeviceInterface = deviceInterface
		} = describeException "failed to create DirectX11 deferred context" $ do
		bk <- newBook
		contextInterface <- book bk $ allocateCOMObject $ createCOMObjectViaPtr $ m_ID3D11Device_CreateDeferredContext deviceInterface 0
		context <- book bk $ dx11CreateContextFromInterface device contextInterface
		return (context, freeBook bk)

	createStaticTexture device@Dx11Device
		{ dx11DeviceInterface = deviceInterface
		} textureInfo@TextureInfo
		{ textureWidth = width
		, textureHeight = height
		, textureDepth = depth
		, textureMips = mips
		, textureFormat = format
		, textureCount = count
		} samplerStateInfo bytes = describeException ("failed to create DirectX11 static texture", textureInfo) $ do
		let dxgiFormat = getDXGIFormat format
		let realCount = if count > 0 then count else 1
		let TextureMetrics
			{ textureImageSize = imageSize
			, textureMipsMetrics = mipsMetrics
			} = calcTextureMetrics textureInfo

		-- create function for creating ID3D11Resource, and shader resource view desc
		(createResource, srvDesc) <- do
			-- fill subresource descs
			let subresourceDesc bytesPtr imageOffset TextureMipMetrics
				{ textureMipOffset = mipOffset
				, textureMipLinePitch = mipLinePitch
				, textureMipSlicePitch = mipSlicePitch
				} = D3D11_SUBRESOURCE_DATA
				{ f_D3D11_SUBRESOURCE_DATA_pSysMem = plusPtr bytesPtr $ imageOffset + mipOffset
				, f_D3D11_SUBRESOURCE_DATA_SysMemPitch = fromIntegral mipLinePitch
				, f_D3D11_SUBRESOURCE_DATA_SysMemSlicePitch = fromIntegral mipSlicePitch
				}
			let subresourceDescs bytesPtr = concatMap (\imageIndex -> map (subresourceDesc bytesPtr $ imageIndex * imageSize) mipsMetrics) [0..(mips - 1)]

			-- switch by texture type
			-- if it's 3D texture
			if depth > 0 then do
				-- arrays of 3D textures not supported
				if count > 0 then throwIO $ DescribeFirstException "array of 3D textures is not supported"
				else return ()
				-- texture desc
				let desc = D3D11_TEXTURE3D_DESC
					{ f_D3D11_TEXTURE3D_DESC_Width = fromIntegral width
					, f_D3D11_TEXTURE3D_DESC_Height = fromIntegral height
					, f_D3D11_TEXTURE3D_DESC_Depth = fromIntegral depth
					, f_D3D11_TEXTURE3D_DESC_MipLevels = fromIntegral mips
					, f_D3D11_TEXTURE3D_DESC_Format = dxgiFormat
					, f_D3D11_TEXTURE3D_DESC_Usage = D3D11_USAGE_IMMUTABLE
					, f_D3D11_TEXTURE3D_DESC_BindFlags = fromIntegral $ fromEnum D3D11_BIND_SHADER_RESOURCE
					, f_D3D11_TEXTURE3D_DESC_CPUAccessFlags = 0
					, f_D3D11_TEXTURE3D_DESC_MiscFlags = 0
					}
				-- create resource function
				let createResource bytesPtr = with desc $ \descPtr -> withArray (subresourceDescs bytesPtr) $ \subresourceDescsPtr -> do
					liftM com_get_ID3D11Resource $ createCOMObjectViaPtr $ m_ID3D11Device_CreateTexture3D deviceInterface descPtr subresourceDescsPtr
				-- SRV desc
				let srvDesc = D3D11_SHADER_RESOURCE_VIEW_DESC_Texture3D
					{ f_D3D11_SHADER_RESOURCE_VIEW_DESC_Format = dxgiFormat
					, f_D3D11_SHADER_RESOURCE_VIEW_DESC_ViewDimension = D3D11_SRV_DIMENSION_TEXTURE3D
					, f_D3D11_SHADER_RESOURCE_VIEW_DESC_Texture3D = D3D11_TEX3D_SRV
						{ f_D3D11_TEX3D_SRV_MostDetailedMip = 0
						, f_D3D11_TEX3D_SRV_MipLevels = -1
						}
					}
				return (createResource, srvDesc)
			-- else if it's 2D texture
			else if width > 0 then do
				-- texture desc
				let desc = D3D11_TEXTURE2D_DESC
					{ f_D3D11_TEXTURE2D_DESC_Width = fromIntegral width
					, f_D3D11_TEXTURE2D_DESC_Height = fromIntegral height
					, f_D3D11_TEXTURE2D_DESC_MipLevels = fromIntegral mips
					, f_D3D11_TEXTURE2D_DESC_ArraySize = fromIntegral realCount
					, f_D3D11_TEXTURE2D_DESC_Format = dxgiFormat
					, f_D3D11_TEXTURE2D_DESC_SampleDesc = DXGI_SAMPLE_DESC
						{ f_DXGI_SAMPLE_DESC_Count = 1
						, f_DXGI_SAMPLE_DESC_Quality = 0
						}
					, f_D3D11_TEXTURE2D_DESC_Usage = D3D11_USAGE_IMMUTABLE
					, f_D3D11_TEXTURE2D_DESC_BindFlags = fromIntegral $ fromEnum D3D11_BIND_SHADER_RESOURCE
					, f_D3D11_TEXTURE2D_DESC_CPUAccessFlags = 0
					, f_D3D11_TEXTURE2D_DESC_MiscFlags = 0
					}
				-- create resource function
				let createResource bytesPtr = with desc $ \descPtr -> withArray (subresourceDescs bytesPtr) $ \subresourceDescsPtr -> do
					liftM com_get_ID3D11Resource $ createCOMObjectViaPtr $ m_ID3D11Device_CreateTexture2D deviceInterface descPtr subresourceDescsPtr
				-- SRV desc
				let srvDesc =
					if count > 0 then D3D11_SHADER_RESOURCE_VIEW_DESC_Texture2DArray
						{ f_D3D11_SHADER_RESOURCE_VIEW_DESC_Format = dxgiFormat
						, f_D3D11_SHADER_RESOURCE_VIEW_DESC_ViewDimension = D3D11_SRV_DIMENSION_TEXTURE2DARRAY
						, f_D3D11_SHADER_RESOURCE_VIEW_DESC_Texture2DArray = D3D11_TEX2D_ARRAY_SRV
							{ f_D3D11_TEX2D_ARRAY_SRV_MostDetailedMip = 0
							, f_D3D11_TEX2D_ARRAY_SRV_MipLevels = -1
							, f_D3D11_TEX2D_ARRAY_SRV_FirstArraySlice = 0
							, f_D3D11_TEX2D_ARRAY_SRV_ArraySize = fromIntegral count
							}
						}
					else D3D11_SHADER_RESOURCE_VIEW_DESC_Texture2D
						{ f_D3D11_SHADER_RESOURCE_VIEW_DESC_Format = dxgiFormat
						, f_D3D11_SHADER_RESOURCE_VIEW_DESC_ViewDimension = D3D11_SRV_DIMENSION_TEXTURE2D
						, f_D3D11_SHADER_RESOURCE_VIEW_DESC_Texture2D = D3D11_TEX2D_SRV
							{ f_D3D11_TEX2D_SRV_MostDetailedMip = 0
							, f_D3D11_TEX2D_SRV_MipLevels = -1
							}
						}
				return (createResource, srvDesc)
			-- else it's 1D texture
			else do
				-- texture desc
				let desc = D3D11_TEXTURE1D_DESC
					{ f_D3D11_TEXTURE1D_DESC_Width = fromIntegral width
					, f_D3D11_TEXTURE1D_DESC_MipLevels = fromIntegral mips
					, f_D3D11_TEXTURE1D_DESC_ArraySize = fromIntegral realCount
					, f_D3D11_TEXTURE1D_DESC_Format = dxgiFormat
					, f_D3D11_TEXTURE1D_DESC_Usage = D3D11_USAGE_IMMUTABLE
					, f_D3D11_TEXTURE1D_DESC_BindFlags = fromIntegral $ fromEnum D3D11_BIND_SHADER_RESOURCE
					, f_D3D11_TEXTURE1D_DESC_CPUAccessFlags = 0
					, f_D3D11_TEXTURE1D_DESC_MiscFlags = 0
					}
				-- create resource function
				let createResource bytesPtr = with desc $ \descPtr -> withArray (subresourceDescs bytesPtr) $ \subresourceDescsPtr -> do
					liftM com_get_ID3D11Resource $ createCOMObjectViaPtr $ m_ID3D11Device_CreateTexture1D deviceInterface descPtr subresourceDescsPtr
				-- SRV desc
				let srvDesc =
					if count > 0 then D3D11_SHADER_RESOURCE_VIEW_DESC_Texture1DArray
						{ f_D3D11_SHADER_RESOURCE_VIEW_DESC_Format = dxgiFormat
						, f_D3D11_SHADER_RESOURCE_VIEW_DESC_ViewDimension = D3D11_SRV_DIMENSION_TEXTURE1DARRAY
						, f_D3D11_SHADER_RESOURCE_VIEW_DESC_Texture1DArray = D3D11_TEX1D_ARRAY_SRV
							{ f_D3D11_TEX1D_ARRAY_SRV_MostDetailedMip = 0
							, f_D3D11_TEX1D_ARRAY_SRV_MipLevels = -1
							, f_D3D11_TEX1D_ARRAY_SRV_FirstArraySlice = 0
							, f_D3D11_TEX1D_ARRAY_SRV_ArraySize = fromIntegral count
							}
						}
					else D3D11_SHADER_RESOURCE_VIEW_DESC_Texture1D
						{ f_D3D11_SHADER_RESOURCE_VIEW_DESC_Format = dxgiFormat
						, f_D3D11_SHADER_RESOURCE_VIEW_DESC_ViewDimension = D3D11_SRV_DIMENSION_TEXTURE1D
						, f_D3D11_SHADER_RESOURCE_VIEW_DESC_Texture1D = D3D11_TEX1D_SRV
							{ f_D3D11_TEX1D_SRV_MostDetailedMip = 0
							, f_D3D11_TEX1D_SRV_MipLevels = -1
							}
						}
				return (createResource, srvDesc)

		-- create ID3D11Resource
		(resourceInterface, releaseResourceInterface) <- allocateCOMObject $ B.unsafeUseAsCString bytes createResource

		bk <- newBook

		-- create ID3D11ShaderResourceView
		srvInterface <- book bk $ allocateCOMObject $ with srvDesc $ \srvDescPtr -> do
			createCOMObjectViaPtr $ m_ID3D11Device_CreateShaderResourceView deviceInterface (pokeCOMObject resourceInterface) srvDescPtr
		-- release resource interface
		releaseResourceInterface

		-- create sampler state
		samplerState <- book bk $ createSamplerState device samplerStateInfo

		return (Dx11TextureId srvInterface samplerState, freeBook bk)

	createSamplerState Dx11Device
		{ dx11DeviceInterface = deviceInterface
		} samplerStateInfo@SamplerStateInfo
		{ samplerMinFilter = minFilter
		, samplerMipFilter = mipFilter
		, samplerMagFilter = magFilter
		, samplerWrapU = wrapU
		, samplerWrapV = wrapV
		, samplerWrapW = wrapW
		, samplerMinLod = minLod
		, samplerMaxLod = maxLod
		, samplerBorderColor = borderColor
		} = describeException ("failed to create DirectX11 sampler state", samplerStateInfo) $ do
		-- conversion function
		let convertWrap wrap = case wrap of
			SamplerWrapRepeat -> D3D11_TEXTURE_ADDRESS_WRAP
			SamplerWrapRepeatMirror -> D3D11_TEXTURE_ADDRESS_MIRROR
			SamplerWrapClamp -> D3D11_TEXTURE_ADDRESS_CLAMP
			SamplerWrapBorder -> D3D11_TEXTURE_ADDRESS_BORDER

		-- desc
		let desc = D3D11_SAMPLER_DESC
			{ f_D3D11_SAMPLER_DESC_Filter = case minFilter of
				SamplerPointFilter -> case magFilter of
					SamplerPointFilter -> case mipFilter of
						SamplerPointFilter -> D3D11_FILTER_MIN_MAG_MIP_POINT
						SamplerLinearFilter -> D3D11_FILTER_MIN_MAG_POINT_MIP_LINEAR
					SamplerLinearFilter -> case mipFilter of
						SamplerPointFilter -> D3D11_FILTER_MIN_POINT_MAG_LINEAR_MIP_POINT
						SamplerLinearFilter -> D3D11_FILTER_MIN_POINT_MAG_MIP_LINEAR
				SamplerLinearFilter -> case magFilter of
					SamplerPointFilter -> case mipFilter of
						SamplerPointFilter -> D3D11_FILTER_MIN_LINEAR_MAG_MIP_POINT
						SamplerLinearFilter -> D3D11_FILTER_MIN_LINEAR_MAG_POINT_MIP_LINEAR
					SamplerLinearFilter -> case mipFilter of
						SamplerPointFilter -> D3D11_FILTER_MIN_MAG_LINEAR_MIP_POINT
						SamplerLinearFilter -> D3D11_FILTER_MIN_MAG_MIP_LINEAR
			, f_D3D11_SAMPLER_DESC_AddressU = convertWrap wrapU
			, f_D3D11_SAMPLER_DESC_AddressV = convertWrap wrapV
			, f_D3D11_SAMPLER_DESC_AddressW = convertWrap wrapW
			, f_D3D11_SAMPLER_DESC_MipLODBias = 0
			, f_D3D11_SAMPLER_DESC_MaxAnisotropy = 16
			, f_D3D11_SAMPLER_DESC_ComparisonFunc = D3D11_COMPARISON_NEVER
			, f_D3D11_SAMPLER_DESC_BorderColor = vecToList borderColor
			, f_D3D11_SAMPLER_DESC_MinLOD = minLod
			, f_D3D11_SAMPLER_DESC_MaxLOD = maxLod
			}

		-- create
		(ssInterface, releaseSsInterface) <- allocateCOMObject $ with desc $ \descPtr -> do
			createCOMObjectViaPtr $ m_ID3D11Device_CreateSamplerState deviceInterface descPtr

		return (Dx11SamplerStateId ssInterface, releaseSsInterface)

	createBlendState Dx11Device
		{ dx11DeviceInterface = deviceInterface
		} blendStateInfo@BlendStateInfo
		{ blendSourceColor = sourceColor
		, blendDestColor = destColor
		, blendColorOperation = colorOperation
		, blendSourceAlpha = sourceAlpha
		, blendDestAlpha = destAlpha
		, blendAlphaOperation = alphaOperation
		} = describeException ("failed to create DirectX11 blend state", blendStateInfo) $ do
		-- conversion functions
		let convertColorSource colorSource = case colorSource of
			ColorSourceZero -> D3D11_BLEND_ZERO
			ColorSourceOne -> D3D11_BLEND_ONE
			ColorSourceSrc -> D3D11_BLEND_SRC_COLOR
			ColorSourceInvSrc -> D3D11_BLEND_INV_SRC_COLOR
			ColorSourceSrcAlpha -> D3D11_BLEND_SRC_ALPHA
			ColorSourceInvSrcAlpha -> D3D11_BLEND_INV_SRC_ALPHA
			ColorSourceDest -> D3D11_BLEND_DEST_COLOR
			ColorSourceInvDest -> D3D11_BLEND_INV_DEST_COLOR
			ColorSourceDestAlpha -> D3D11_BLEND_DEST_ALPHA
			ColorSourceInvDestAlpha -> D3D11_BLEND_INV_DEST_ALPHA
			ColorSourceSecondSrc -> D3D11_BLEND_SRC1_COLOR
			ColorSourceInvSecondSrc -> D3D11_BLEND_INV_SRC1_COLOR
			ColorSourceSecondSrcAlpha -> D3D11_BLEND_SRC1_ALPHA
			ColorSourceInvSecondSrcAlpha -> D3D11_BLEND_INV_SRC1_ALPHA
		let convertAlphaSource alphaSource = case alphaSource of
			AlphaSourceZero -> D3D11_BLEND_ZERO
			AlphaSourceOne -> D3D11_BLEND_ONE
			AlphaSourceSrc -> D3D11_BLEND_SRC_ALPHA
			AlphaSourceInvSrc -> D3D11_BLEND_INV_SRC_ALPHA
			AlphaSourceDest -> D3D11_BLEND_DEST_ALPHA
			AlphaSourceInvDest -> D3D11_BLEND_INV_DEST_ALPHA
			AlphaSourceSecondSrc -> D3D11_BLEND_SRC1_ALPHA
			AlphaSourceInvSecondSrc -> D3D11_BLEND_INV_SRC1_ALPHA
		let convertOperation operation = case operation of
			BlendOperationAdd -> D3D11_BLEND_OP_ADD
			BlendOperationSubtractAB -> D3D11_BLEND_OP_REV_SUBTRACT
			BlendOperationSubtractBA -> D3D11_BLEND_OP_SUBTRACT
			BlendOperationMin -> D3D11_BLEND_OP_MIN
			BlendOperationMax -> D3D11_BLEND_OP_MAX

		-- desc
		let desc = D3D11_BLEND_DESC
			{ f_D3D11_BLEND_DESC_AlphaToCoverageEnable = False
			, f_D3D11_BLEND_DESC_IndependentBlendEnable = False
			, f_D3D11_BLEND_DESC_RenderTarget = replicate 8 D3D11_RENDER_TARGET_BLEND_DESC
				{ f_D3D11_RENDER_TARGET_BLEND_DESC_BlendEnable = True
				, f_D3D11_RENDER_TARGET_BLEND_DESC_SrcBlend = convertColorSource sourceColor
				, f_D3D11_RENDER_TARGET_BLEND_DESC_DestBlend = convertColorSource destColor
				, f_D3D11_RENDER_TARGET_BLEND_DESC_BlendOp = convertOperation colorOperation
				, f_D3D11_RENDER_TARGET_BLEND_DESC_SrcBlendAlpha = convertAlphaSource sourceAlpha
				, f_D3D11_RENDER_TARGET_BLEND_DESC_DestBlendAlpha = convertAlphaSource destAlpha
				, f_D3D11_RENDER_TARGET_BLEND_DESC_BlendOpAlpha = convertOperation alphaOperation
				, f_D3D11_RENDER_TARGET_BLEND_DESC_RenderTargetWriteMask = 15 -- D3D11_COLOR_WRITE_ENABLE_ALL
				}
			}

		-- create
		(bsInterface, releaseBsInterface) <- allocateCOMObject $ with desc $ \descPtr -> do
			createCOMObjectViaPtr $ m_ID3D11Device_CreateBlendState deviceInterface descPtr

		return (Dx11BlendStateId bsInterface, releaseBsInterface)

	createReadableRenderTarget device@Dx11Device
		{ dx11DeviceInterface = deviceInterface
		} width height format samplerStateInfo = describeException ("failed to create DirectX11 readable render target", width, height, format) $ do

		let dxgiFormat = getDXGIFormat format

		-- resource desc
		let desc = D3D11_TEXTURE2D_DESC
			{ f_D3D11_TEXTURE2D_DESC_Width = fromIntegral width
			, f_D3D11_TEXTURE2D_DESC_Height = fromIntegral height
			, f_D3D11_TEXTURE2D_DESC_MipLevels = 1
			, f_D3D11_TEXTURE2D_DESC_ArraySize = 1
			, f_D3D11_TEXTURE2D_DESC_Format = dxgiFormat
			, f_D3D11_TEXTURE2D_DESC_SampleDesc = DXGI_SAMPLE_DESC
				{ f_DXGI_SAMPLE_DESC_Count = 1
				, f_DXGI_SAMPLE_DESC_Quality = 0
				}
			, f_D3D11_TEXTURE2D_DESC_Usage = D3D11_USAGE_DEFAULT
			, f_D3D11_TEXTURE2D_DESC_BindFlags = fromIntegral ((fromEnum D3D11_BIND_RENDER_TARGET) .|. (fromEnum D3D11_BIND_SHADER_RESOURCE))
			, f_D3D11_TEXTURE2D_DESC_CPUAccessFlags = 0
			, f_D3D11_TEXTURE2D_DESC_MiscFlags = 0
			}
		-- create resource
		(resourceInterface, releaseResourceInterface) <- allocateCOMObject $ with desc $ \descPtr -> do
			liftM com_get_ID3D11Resource $ createCOMObjectViaPtr $ m_ID3D11Device_CreateTexture2D deviceInterface descPtr nullPtr

		bk <- newBook

		-- create render target view
		rtvInterface <- book bk $ allocateCOMObject $ createCOMObjectViaPtr $ m_ID3D11Device_CreateRenderTargetView deviceInterface (pokeCOMObject resourceInterface) nullPtr

		-- create shader resource view
		srvInterface <- book bk $ allocateCOMObject $ createCOMObjectViaPtr $ m_ID3D11Device_CreateShaderResourceView deviceInterface (pokeCOMObject resourceInterface) nullPtr

		-- release resource interface
		releaseResourceInterface

		-- create sampler state
		samplerState <- book bk $ createSamplerState device samplerStateInfo

		return ((Dx11RenderTargetId rtvInterface, Dx11TextureId srvInterface samplerState), freeBook bk)

	createDepthStencilTarget Dx11Device
		{ dx11DeviceInterface = deviceInterface
		} width height = describeException ("failed to create DirectX11 depth stencil target", width, height) $ do

		-- resource desc
		let desc = D3D11_TEXTURE2D_DESC
			{ f_D3D11_TEXTURE2D_DESC_Width = fromIntegral width
			, f_D3D11_TEXTURE2D_DESC_Height = fromIntegral height
			, f_D3D11_TEXTURE2D_DESC_MipLevels = 1
			, f_D3D11_TEXTURE2D_DESC_ArraySize = 1
			, f_D3D11_TEXTURE2D_DESC_Format = DXGI_FORMAT_R24G8_TYPELESS
			, f_D3D11_TEXTURE2D_DESC_SampleDesc = DXGI_SAMPLE_DESC
				{ f_DXGI_SAMPLE_DESC_Count = 1
				, f_DXGI_SAMPLE_DESC_Quality = 0
				}
			, f_D3D11_TEXTURE2D_DESC_Usage = D3D11_USAGE_DEFAULT
			, f_D3D11_TEXTURE2D_DESC_BindFlags = fromIntegral $ fromEnum D3D11_BIND_DEPTH_STENCIL
			, f_D3D11_TEXTURE2D_DESC_CPUAccessFlags = 0
			, f_D3D11_TEXTURE2D_DESC_MiscFlags = 0
			}
		-- create resource
		(resourceInterface, releaseResourceInterface) <- allocateCOMObject $ with desc $ \descPtr -> do
			liftM com_get_ID3D11Resource $ createCOMObjectViaPtr $ m_ID3D11Device_CreateTexture2D deviceInterface descPtr nullPtr

		-- DSV desc
		let dsvDesc = D3D11_DEPTH_STENCIL_VIEW_DESC_Texture2D
			{ f_D3D11_DEPTH_STENCIL_VIEW_DESC_Format = DXGI_FORMAT_D24_UNORM_S8_UINT
			, f_D3D11_DEPTH_STENCIL_VIEW_DESC_ViewDimension = D3D11_DSV_DIMENSION_TEXTURE2D
			, f_D3D11_DEPTH_STENCIL_VIEW_DESC_Flags = 0
			, f_D3D11_DEPTH_STENCIL_VIEW_DESC_Texture2D = D3D11_TEX2D_DSV
				{ f_D3D11_TEX2D_DSV_MipSlice = 0
				}
			}
		-- create depth stencil view
		(dsvInterface, releaseDsvInterface) <- allocateCOMObject $ with dsvDesc $ \dsvDescPtr -> do
			createCOMObjectViaPtr $ m_ID3D11Device_CreateDepthStencilView deviceInterface (pokeCOMObject resourceInterface) dsvDescPtr

		-- release resource interface
		releaseResourceInterface

		return (Dx11DepthStencilTargetId dsvInterface, releaseDsvInterface)

	createReadableDepthStencilTarget Dx11Device
		{ dx11DeviceInterface = deviceInterface
		} width height = describeException ("failed to create DirectX11 readable depth stencil target", width, height) $ do

		-- resource desc
		let desc = D3D11_TEXTURE2D_DESC
			{ f_D3D11_TEXTURE2D_DESC_Width = fromIntegral width
			, f_D3D11_TEXTURE2D_DESC_Height = fromIntegral height
			, f_D3D11_TEXTURE2D_DESC_MipLevels = 1
			, f_D3D11_TEXTURE2D_DESC_ArraySize = 1
			, f_D3D11_TEXTURE2D_DESC_Format = DXGI_FORMAT_R24G8_TYPELESS
			, f_D3D11_TEXTURE2D_DESC_SampleDesc = DXGI_SAMPLE_DESC
				{ f_DXGI_SAMPLE_DESC_Count = 1
				, f_DXGI_SAMPLE_DESC_Quality = 0
				}
			, f_D3D11_TEXTURE2D_DESC_Usage = D3D11_USAGE_DEFAULT
			, f_D3D11_TEXTURE2D_DESC_BindFlags = fromIntegral ((fromEnum D3D11_BIND_DEPTH_STENCIL) .|. (fromEnum D3D11_BIND_SHADER_RESOURCE))
			, f_D3D11_TEXTURE2D_DESC_CPUAccessFlags = 0
			, f_D3D11_TEXTURE2D_DESC_MiscFlags = 0
			}
		-- create resource
		(resourceInterface, releaseResourceInterface) <- allocateCOMObject $ with desc $ \descPtr -> do
			liftM com_get_ID3D11Resource $ createCOMObjectViaPtr $ m_ID3D11Device_CreateTexture2D deviceInterface descPtr nullPtr

		-- DSV desc
		let dsvDesc = D3D11_DEPTH_STENCIL_VIEW_DESC_Texture2D
			{ f_D3D11_DEPTH_STENCIL_VIEW_DESC_Format = DXGI_FORMAT_D24_UNORM_S8_UINT
			, f_D3D11_DEPTH_STENCIL_VIEW_DESC_ViewDimension = D3D11_DSV_DIMENSION_TEXTURE2D
			, f_D3D11_DEPTH_STENCIL_VIEW_DESC_Flags = 0
			, f_D3D11_DEPTH_STENCIL_VIEW_DESC_Texture2D = D3D11_TEX2D_DSV
				{ f_D3D11_TEX2D_DSV_MipSlice = 0
				}
			}
		-- create depth stencil view
		(dsvInterface, releaseDsvInterface) <- allocateCOMObject $ with dsvDesc $ \dsvDescPtr -> do
			createCOMObjectViaPtr $ m_ID3D11Device_CreateDepthStencilView deviceInterface (pokeCOMObject resourceInterface) dsvDescPtr

		-- SRV desc
		let srvDesc = D3D11_SHADER_RESOURCE_VIEW_DESC_Texture2D
			{ f_D3D11_SHADER_RESOURCE_VIEW_DESC_Format = DXGI_FORMAT_R24_UNORM_X8_TYPELESS
			, f_D3D11_SHADER_RESOURCE_VIEW_DESC_ViewDimension = D3D11_SRV_DIMENSION_TEXTURE2D
			, f_D3D11_SHADER_RESOURCE_VIEW_DESC_Texture2D = D3D11_TEX2D_SRV
				{ f_D3D11_TEX2D_SRV_MostDetailedMip = 0
				, f_D3D11_TEX2D_SRV_MipLevels = 1
				}
			}
		-- create shader resource view
		(srvInterface, releaseSrvInterface) <- allocateCOMObject $ with srvDesc $ \srvDescPtr -> do
			createCOMObjectViaPtr $ m_ID3D11Device_CreateShaderResourceView deviceInterface (pokeCOMObject resourceInterface) srvDescPtr

		-- release resource interface
		releaseResourceInterface

		let destroy = do
			releaseDsvInterface
			releaseSrvInterface

		return ((Dx11DepthStencilTargetId dsvInterface, Dx11TextureId srvInterface nullSamplerState), destroy)

	createFrameBuffer _device renderTargets depthStencilTarget = do
		return (Dx11FrameBufferId renderTargets depthStencilTarget, return ())

	createStaticVertexBuffer Dx11Device
		{ dx11DeviceInterface = deviceInterface
		} bytes stride = describeException "failed to create DirectX11 static vertex buffer" $ do
		-- desc
		let desc = D3D11_BUFFER_DESC
			{ f_D3D11_BUFFER_DESC_ByteWidth = fromIntegral $ B.length bytes
			, f_D3D11_BUFFER_DESC_Usage = D3D11_USAGE_IMMUTABLE
			, f_D3D11_BUFFER_DESC_BindFlags = fromIntegral $ fromEnum D3D11_BIND_VERTEX_BUFFER
			, f_D3D11_BUFFER_DESC_CPUAccessFlags = 0
			, f_D3D11_BUFFER_DESC_MiscFlags = 0
			, f_D3D11_BUFFER_DESC_StructureByteStride = 0
			}
		-- data
		let subresourceData bytesPtr = D3D11_SUBRESOURCE_DATA
			{ f_D3D11_SUBRESOURCE_DATA_pSysMem = bytesPtr
			, f_D3D11_SUBRESOURCE_DATA_SysMemPitch = 0
			, f_D3D11_SUBRESOURCE_DATA_SysMemSlicePitch = 0
			}
		-- create
		(bufferInterface, releaseBufferInterface) <- allocateCOMObject $ with desc $ \descPtr -> do
			B.unsafeUseAsCString bytes $ \bytesPtr -> do
				with (subresourceData $ castPtr bytesPtr) $ \subresourceDataPtr -> do
					createCOMObjectViaPtr $ m_ID3D11Device_CreateBuffer deviceInterface descPtr subresourceDataPtr

		return (Dx11VertexBufferId bufferInterface stride, releaseBufferInterface)

	createDynamicVertexBuffer Dx11Device
		{ dx11DeviceInterface = deviceInterface
		} size stride = describeException "failed to create DirectX11 dynamic vertex buffer" $ do
		-- desc
		let desc = D3D11_BUFFER_DESC
			{ f_D3D11_BUFFER_DESC_ByteWidth = fromIntegral size
			, f_D3D11_BUFFER_DESC_Usage = D3D11_USAGE_DYNAMIC
			, f_D3D11_BUFFER_DESC_BindFlags = fromIntegral $ fromEnum D3D11_BIND_VERTEX_BUFFER
			, f_D3D11_BUFFER_DESC_CPUAccessFlags = fromIntegral $ fromEnum D3D11_CPU_ACCESS_WRITE
			, f_D3D11_BUFFER_DESC_MiscFlags = 0
			, f_D3D11_BUFFER_DESC_StructureByteStride = 0
			}
		-- create
		(bufferInterface, releaseBufferInterface) <- allocateCOMObject $ with desc $ \descPtr -> do
			createCOMObjectViaPtr $ m_ID3D11Device_CreateBuffer deviceInterface descPtr nullPtr

		return (Dx11VertexBufferId bufferInterface stride, releaseBufferInterface)

	createStaticIndexBuffer Dx11Device
		{ dx11DeviceInterface = deviceInterface
		} bytes is32bit = describeException "failed to create DirectX11 index buffer" $ do
		-- desc
		let desc = D3D11_BUFFER_DESC
			{ f_D3D11_BUFFER_DESC_ByteWidth = fromIntegral $ B.length bytes
			, f_D3D11_BUFFER_DESC_Usage = D3D11_USAGE_IMMUTABLE
			, f_D3D11_BUFFER_DESC_BindFlags = fromIntegral $ fromEnum D3D11_BIND_INDEX_BUFFER
			, f_D3D11_BUFFER_DESC_CPUAccessFlags = 0
			, f_D3D11_BUFFER_DESC_MiscFlags = 0
			, f_D3D11_BUFFER_DESC_StructureByteStride = 0
			}
		-- data
		let subresourceData bytesPtr = D3D11_SUBRESOURCE_DATA
			{ f_D3D11_SUBRESOURCE_DATA_pSysMem = bytesPtr
			, f_D3D11_SUBRESOURCE_DATA_SysMemPitch = 0
			, f_D3D11_SUBRESOURCE_DATA_SysMemSlicePitch = 0
			}
		-- create
		(bufferInterface, releaseBufferInterface) <- allocateCOMObject $ with desc $ \descPtr -> do
			B.unsafeUseAsCString bytes $ \bytesPtr -> do
				with (subresourceData $ castPtr bytesPtr) $ \subresourceDataPtr -> do
					createCOMObjectViaPtr $ m_ID3D11Device_CreateBuffer deviceInterface descPtr subresourceDataPtr

		return
			( (Dx11IndexBufferId bufferInterface (if is32bit then DXGI_FORMAT_R32_UINT else DXGI_FORMAT_R16_UINT) D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST)
			, releaseBufferInterface
			)

	createProgram Dx11Device
		{ dx11DeviceInterface = deviceInterface
		, dx11DeviceD3DCompile = d3dCompile
		, dx11DeviceDebug = debug
		} _binaryCache program = describeException "failed to create DirectX11 program" $ do

		-- function to create input layout
		let createInputLayout attributes vertexShaderByteCode = allocateCOMObject $ B.unsafeUseAsCStringLen vertexShaderByteCode $ \(byteCodePtr, byteCodeSize) -> let
			inputElementDescs (HlslAttribute
				{ hlslAttributeSemantic = semantic
				, hlslAttributeInfo = Attribute
					{ attributeSlot = slot
					, attributeOffset = offset
					, attributeDivisor = divisor
					, attributeType = atype
					}
				} : restAttributes) descs = do
				B.useAsCString (T.encodeUtf8 semantic) $ \semanticPtr -> do
					let format = convertFormat atype
					if format == DXGI_FORMAT_UNKNOWN then fail $ "wrong attribute format for " ++ T.unpack semantic
					else return ()
					let desc = D3D11_INPUT_ELEMENT_DESC
						{ f_D3D11_INPUT_ELEMENT_DESC_SemanticName = semanticPtr
						, f_D3D11_INPUT_ELEMENT_DESC_SemanticIndex = 0
						, f_D3D11_INPUT_ELEMENT_DESC_Format = format
						, f_D3D11_INPUT_ELEMENT_DESC_InputSlot = fromIntegral slot
						, f_D3D11_INPUT_ELEMENT_DESC_AlignedByteOffset = fromIntegral offset
						, f_D3D11_INPUT_ELEMENT_DESC_InputSlotClass = if divisor > 0 then D3D11_INPUT_PER_INSTANCE_DATA else D3D11_INPUT_PER_VERTEX_DATA
						, f_D3D11_INPUT_ELEMENT_DESC_InstanceDataStepRate = fromIntegral divisor
						}
					inputElementDescs restAttributes (desc : descs)
			inputElementDescs [] descs = do
				withArray descs $ \descsPtr -> do
					createCOMObjectViaPtr $ m_ID3D11Device_CreateInputLayout deviceInterface descsPtr (fromIntegral $ length descs) (castPtr byteCodePtr) (fromIntegral byteCodeSize)
			convertFormat atype = case atype of
				ATFloat32 -> DXGI_FORMAT_R32_FLOAT
				ATFloat16 -> DXGI_FORMAT_R16_FLOAT
				ATInt32 NonNormalized -> DXGI_FORMAT_R32_SINT
				ATInt16 NonNormalized -> DXGI_FORMAT_R16_SINT
				ATInt16 Normalized -> DXGI_FORMAT_R16_SNORM
				ATInt8 NonNormalized -> DXGI_FORMAT_R8_SINT
				ATInt8 Normalized -> DXGI_FORMAT_R8_SNORM
				ATUint32 NonNormalized -> DXGI_FORMAT_R32_UINT
				ATUint16 NonNormalized -> DXGI_FORMAT_R16_UINT
				ATUint16 Normalized -> DXGI_FORMAT_R16_UNORM
				ATUint8 NonNormalized -> DXGI_FORMAT_R8_UINT
				ATUint8 Normalized -> DXGI_FORMAT_R8_UNORM
				ATVec1 a -> convertFormat a
				ATVec2 ATFloat32 -> DXGI_FORMAT_R32G32_FLOAT
				ATVec2 ATFloat16 -> DXGI_FORMAT_R16G16_FLOAT
				ATVec2 (ATInt32 NonNormalized) -> DXGI_FORMAT_R32G32_SINT
				ATVec2 (ATInt16 NonNormalized) -> DXGI_FORMAT_R16G16_SINT
				ATVec2 (ATInt16 Normalized) -> DXGI_FORMAT_R16G16_SNORM
				ATVec2 (ATInt8 NonNormalized) -> DXGI_FORMAT_R8G8_SINT
				ATVec2 (ATInt8 Normalized) -> DXGI_FORMAT_R8G8_SNORM
				ATVec2 (ATUint32 NonNormalized) -> DXGI_FORMAT_R32G32_UINT
				ATVec2 (ATUint16 NonNormalized) -> DXGI_FORMAT_R16G16_UINT
				ATVec2 (ATUint16 Normalized) -> DXGI_FORMAT_R16G16_UNORM
				ATVec2 (ATUint8 NonNormalized) -> DXGI_FORMAT_R8G8_UINT
				ATVec2 (ATUint8 Normalized) -> DXGI_FORMAT_R8G8_UNORM
				ATVec3 ATFloat32 -> DXGI_FORMAT_R32G32B32_FLOAT
				ATVec3 (ATInt32 NonNormalized) -> DXGI_FORMAT_R32G32B32_SINT
				ATVec3 (ATUint32 NonNormalized) -> DXGI_FORMAT_R32G32B32_UINT
				ATVec4 ATFloat32 -> DXGI_FORMAT_R32G32B32A32_FLOAT
				ATVec4 ATFloat16 -> DXGI_FORMAT_R16G16B16A16_FLOAT
				ATVec4 (ATInt32 NonNormalized) -> DXGI_FORMAT_R32G32B32A32_SINT
				ATVec4 (ATInt16 NonNormalized) -> DXGI_FORMAT_R16G16B16A16_SINT
				ATVec4 (ATInt16 Normalized) -> DXGI_FORMAT_R16G16B16A16_SNORM
				ATVec4 (ATInt8 NonNormalized) -> DXGI_FORMAT_R8G8B8A8_SINT
				ATVec4 (ATInt8 Normalized) -> DXGI_FORMAT_R8G8B8A8_SNORM
				ATVec4 (ATUint32 NonNormalized) -> DXGI_FORMAT_R32G32B32A32_UINT
				ATVec4 (ATUint16 NonNormalized) -> DXGI_FORMAT_R16G16B16A16_UINT
				ATVec4 (ATUint16 Normalized) -> DXGI_FORMAT_R16G16B16A16_UNORM
				ATVec4 (ATUint8 NonNormalized) -> DXGI_FORMAT_R8G8B8A8_UINT
				ATVec4 (ATUint8 Normalized) -> DXGI_FORMAT_R8G8B8A8_UNORM
				_ -> DXGI_FORMAT_UNKNOWN
			in inputElementDescs attributes []

		-- function to compile shader
		let compileShader HlslShader
			{ hlslShaderSource = source
			, hlslShaderEntryPoint = entryPoint
			, hlslShaderProfile = profile
			} = describeException ("failed to compile shader", source, entryPoint, profile) $ do
			let optimize = not debug
			let flags = fromIntegral $ (fromEnum D3DCOMPILE_ENABLE_STRICTNESS) .|. (fromEnum D3DCOMPILE_PACK_MATRIX_COLUMN_MAJOR)
				.|. (if debug then fromEnum D3DCOMPILE_DEBUG else 0)
				.|. (if optimize then fromEnum D3DCOMPILE_OPTIMIZATION_LEVEL3 else (fromEnum D3DCOMPILE_OPTIMIZATION_LEVEL0) .|. (fromEnum D3DCOMPILE_SKIP_OPTIMIZATION))
			(hr, shaderData, errorsData) <- B.unsafeUseAsCStringLen (T.encodeUtf8 source) $ \(sourcePtr, sourceLen) -> do
				B.useAsCString (T.encodeUtf8 entryPoint) $ \entryPointPtr -> do
					B.useAsCString (T.encodeUtf8 profile) $ \profilePtr -> do
						alloca $ \shaderBlobPtrPtr -> do
							alloca $ \errorsBlobPtrPtr -> do
								hr <- d3dCompile
									(castPtr sourcePtr) -- pSrcData
									(fromIntegral sourceLen) -- SrcDataSize
									nullPtr -- pSourceName
									nullPtr -- pDefines
									nullPtr -- pInclude
									entryPointPtr -- pEntrypoint
									profilePtr -- pTarget
									flags -- Flags1
									0 -- Flags2
									shaderBlobPtrPtr -- ppCode
									errorsBlobPtrPtr -- ppErrorMsgs
								-- retrieve blobs
								let getBlobData blobPtrPtr = do
									blobPtr <- peek blobPtrPtr
									if blobPtr == nullPtr then return B.empty
									else do
										blobInterface <- peekCOMObject blobPtr
										dataPtr <- m_ID3DBlob_GetBufferPointer blobInterface
										dataSize <- liftM fromIntegral $ m_ID3DBlob_GetBufferSize blobInterface
										dataCopyPtr <- mallocBytes dataSize
										copyArray dataCopyPtr (castPtr dataPtr) dataSize
										_ <- m_IUnknown_Release blobInterface
										B.unsafePackMallocCStringLen (dataCopyPtr, dataSize)
								shaderData <- getBlobData shaderBlobPtrPtr
								errorsData <- getBlobData errorsBlobPtrPtr
								return (hr, shaderData, errorsData)
			if hresultFailed hr then fail $ "failed to compile shader: " ++ BC.unpack errorsData
			else return shaderData

		-- function to create vertex shader
		let createVertexShader bytecode = allocateCOMObject $ do
			B.unsafeUseAsCStringLen bytecode $ \(ptr, len) -> do
				describeException "failed to create vertex shader" $ do
					createCOMObjectViaPtr $ m_ID3D11Device_CreateVertexShader deviceInterface (castPtr ptr) (fromIntegral len) nullPtr
		-- function to create pixel shader
		let createPixelShader bytecode = allocateCOMObject $ do
			B.unsafeUseAsCStringLen bytecode $ \(ptr, len) -> do
				describeException "failed to create pixel shader" $ do
					createCOMObjectViaPtr $ m_ID3D11Device_CreatePixelShader deviceInterface (castPtr ptr) (fromIntegral len) nullPtr

		-- generate HLSL
		HlslProgram
			{ hlslProgramAttributes = attributes
			, hlslProgramShaders = shaders
			} <- liftM hlslGenerateProgram $ runProgram program

		bk <- newBook

		-- sort shaders by stage
		let sortedShaders = sortBy (\(stage1, _) (stage2, _) -> compare stage1 stage2) shaders
		-- accept only certain combinations of shaders
		case sortedShaders of
			[(HlslVertexStage, vertexShader), (HlslPixelStage, pixelShader)] -> do
				vertexShaderByteCode <- compileShader vertexShader
				inputLayoutInterface <- book bk $ createInputLayout attributes vertexShaderByteCode
				vertexShaderInterface <- book bk $ createVertexShader vertexShaderByteCode
				pixelShaderInterface <- book bk $ createPixelShader =<< compileShader pixelShader
				return (Dx11VertexPixelProgramId inputLayoutInterface vertexShaderInterface pixelShaderInterface, freeBook bk)
			_ -> throwIO $ DescribeFirstException ("unsupported combination of HLSL shaders", map fst sortedShaders)

	createUniformBuffer Dx11Device
		{ dx11DeviceInterface = deviceInterface
		} size = describeException "failed to create DirectX11 uniform buffer" $ do
		-- round up size to be divisible by sizeof(vec4)
		-- also make it non-zero
		let roundedSize = ((if size > 0 then size else 1) + 15) .&. (complement 15)

		-- desc
		let desc = D3D11_BUFFER_DESC
			{ f_D3D11_BUFFER_DESC_ByteWidth = fromIntegral roundedSize
			, f_D3D11_BUFFER_DESC_Usage = D3D11_USAGE_DYNAMIC
			, f_D3D11_BUFFER_DESC_BindFlags = fromIntegral $ fromEnum D3D11_BIND_CONSTANT_BUFFER
			, f_D3D11_BUFFER_DESC_CPUAccessFlags = fromIntegral $ fromEnum D3D11_CPU_ACCESS_WRITE
			, f_D3D11_BUFFER_DESC_MiscFlags = 0
			, f_D3D11_BUFFER_DESC_StructureByteStride = 0
			}

		-- create
		(bufferInterface, releaseBufferInterface) <- allocateCOMObject $ with desc $ \descPtr -> do
			createCOMObjectViaPtr $ m_ID3D11Device_CreateBuffer deviceInterface descPtr nullPtr

		return (Dx11UniformBufferId bufferInterface, releaseBufferInterface)

-- | Create DirectX11 device.
dx11CreateDevice :: DeviceId DXGISystem -> Bool -> IO ((Dx11Device, Dx11Context), IO ())
dx11CreateDevice (DXGIDeviceId system adapter) debug = describeException "failed to create DirectX11 graphics device" $ withSpecialBook $ \bk -> do

	-- create device and context interfaces
	(deviceInterface, contextInterface) <- book bk $ alloca $ \devicePtr -> alloca $ \deviceContextPtr -> do
		let featureLevels = [D3D_FEATURE_LEVEL_11_1, D3D_FEATURE_LEVEL_11_0, D3D_FEATURE_LEVEL_10_1, D3D_FEATURE_LEVEL_10_0]
		withArray featureLevels $ \featureLevelsPtr -> alloca $ \realFeatureLevelPtr -> do
			describeException "failed to create D3D11 device" $ do
				-- driver type is required to be D3D_DRIVER_TYPE_UNKNOWN, because adapter is not null
				-- see http://msdn.microsoft.com/en-us/library/ff476082 (remarks)
				hresultCheck =<< d3d11CreateDevice (pokeCOMObject adapter) (wrapEnum D3D_DRIVER_TYPE_UNKNOWN) nullPtr 0 featureLevelsPtr (fromIntegral $ length featureLevels) d3d11SdkVersion devicePtr realFeatureLevelPtr deviceContextPtr

		deviceInterface <- peekCOMObject =<< peek devicePtr
		contextInterface <- peekCOMObject =<< peek deviceContextPtr

		let destroy = do
			void $ m_IUnknown_Release deviceInterface
			void $ m_IUnknown_Release contextInterface

		return ((deviceInterface, contextInterface), destroy)

	-- load shader compiler and get compile function
	d3dCompileProc <- liftM mkD3DCompile $ loadLibraryAndGetProcAddress "D3DCompiler_43.dll" "D3DCompile"

	let device = Dx11Device
		{ dx11DeviceSystem = system
		, dx11DeviceInterface = deviceInterface
		, dx11DeviceImmediateContext = contextInterface
		, dx11DeviceD3DCompile = d3dCompileProc
		, dx11DeviceDebug = debug
		}

	-- create context from interface
	context <- book bk $ dx11CreateContextFromInterface device contextInterface

	-- create and set rasterizer state, in order to enable scissor
	-- we don't use rasterizer state for something else yet, so just set it into context and forget
	do
		let desc = D3D11_RASTERIZER_DESC
			{ f_D3D11_RASTERIZER_DESC_FillMode = D3D11_FILL_SOLID
			, f_D3D11_RASTERIZER_DESC_CullMode = D3D11_CULL_BACK
			, f_D3D11_RASTERIZER_DESC_FrontCounterClockwise = False
			, f_D3D11_RASTERIZER_DESC_DepthBias = 0
			, f_D3D11_RASTERIZER_DESC_SlopeScaledDepthBias = 0
			, f_D3D11_RASTERIZER_DESC_DepthBiasClamp = 0
			, f_D3D11_RASTERIZER_DESC_DepthClipEnable = True
			, f_D3D11_RASTERIZER_DESC_ScissorEnable = True
			, f_D3D11_RASTERIZER_DESC_MultisampleEnable = False
			, f_D3D11_RASTERIZER_DESC_AntialiasedLineEnable = False
			}
		rasterizerStateInterface <- book bk $ allocateCOMObject $ with desc $ \descPtr -> createCOMObjectViaPtr $ m_ID3D11Device_CreateRasterizerState deviceInterface descPtr
		m_ID3D11DeviceContext_RSSetState contextInterface (pokeCOMObject rasterizerStateInterface)

	return (device, context)

-- | DirectX11 graphics context.
data Dx11Context = Dx11Context
	{ dx11ContextDevice :: Dx11Device
	, dx11ContextInterface :: ID3D11DeviceContext
	-- | Context state corresponding to the ID3D11DeviceContext state.
	, dx11ContextActualState :: !Dx11ContextState
	-- | Desired context state set by user.
	, dx11ContextDesiredState :: !Dx11ContextState
	-- | Cache of depth stencil states. Not in device, to be able to use IORef for performance.
	, dx11ContextDepthStencilStateCache :: !(IORef (HM.HashMap Int ID3D11DepthStencilState))
	}

data Dx11ContextState = Dx11ContextState
	{ dx11ContextStateFrameBuffer :: !(IORef (FrameBufferId Dx11Device))
	, dx11ContextStateViewport :: !(IORef Int4)
	, dx11ContextStateScissor :: !(IORef (Maybe Int4))
	, dx11ContextStateVertexBuffers :: !(VM.IOVector (VertexBufferId Dx11Device))
	, dx11ContextStateIndexBuffer :: !(IORef (IndexBufferId Dx11Device))
	, dx11ContextStateUniformBuffers :: !(VM.IOVector (UniformBufferId Dx11Device))
	, dx11ContextStateSamplers :: !(VM.IOVector (TextureId Dx11Device, SamplerStateId Dx11Device))
	, dx11ContextStateBlendState :: !(IORef (BlendStateId Dx11Device))
	, dx11ContextStateDepthTestFunc :: !(IORef DepthTestFunc)
	, dx11ContextStateDepthWrite :: !(IORef Bool)
	, dx11ContextStateProgram :: !(IORef (ProgramId Dx11Device))
	}

dx11CreateContextFromInterface :: Dx11Device -> ID3D11DeviceContext -> IO (Dx11Context, IO ())
dx11CreateContextFromInterface device contextInterface = do
	actualContextState <- dx11CreateContextState
	desiredContextState <- dx11CreateContextState
	depthStencilStateCacheRef <- newIORef HM.empty
	let destroy = do
		depthStencilStateCache <- readIORef depthStencilStateCacheRef
		forM_ (HM.toList depthStencilStateCache) $ \(_code, depthStencilStateInterface) -> do
			m_IUnknown_Release depthStencilStateInterface
		writeIORef depthStencilStateCacheRef HM.empty
	return (Dx11Context
		{ dx11ContextDevice = device
		, dx11ContextInterface = contextInterface
		, dx11ContextActualState = actualContextState
		, dx11ContextDesiredState = desiredContextState
		, dx11ContextDepthStencilStateCache = depthStencilStateCacheRef
		}, destroy)

dx11CreateContextState :: IO Dx11ContextState
dx11CreateContextState = do
	frameBuffer <- newIORef $ Dx11FrameBufferId [] Dx11NullDepthStencilTargetId
	viewport <- newIORef $ Vec4 0 0 0 0
	scissor <- newIORef Nothing
	vertexBuffers <- VM.replicate 8 Dx11NullVertexBufferId
	indexBuffer <- newIORef $ Dx11NullIndexBufferId D3D11_PRIMITIVE_TOPOLOGY_UNDEFINED
	uniformBuffers <- VM.replicate 8 Dx11NullUniformBufferId
	samplers <- VM.replicate 8 (Dx11NullTextureId, Dx11NullSamplerStateId)
	blendState <- newIORef $ Dx11NullBlendStateId
	depthTestFunc <- newIORef $ DepthTestFuncLess
	depthWrite <- newIORef True
	program <- newIORef Dx11NullProgramId
	return Dx11ContextState
		{ dx11ContextStateFrameBuffer = frameBuffer
		, dx11ContextStateViewport = viewport
		, dx11ContextStateScissor = scissor
		, dx11ContextStateVertexBuffers = vertexBuffers
		, dx11ContextStateIndexBuffer = indexBuffer
		, dx11ContextStateUniformBuffers = uniformBuffers
		, dx11ContextStateSamplers = samplers
		, dx11ContextStateBlendState = blendState
		, dx11ContextStateDepthTestFunc = depthTestFunc
		, dx11ContextStateDepthWrite = depthWrite
		, dx11ContextStateProgram = program
		}

dx11SetDefaultContextState :: Dx11ContextState -> IO ()
dx11SetDefaultContextState Dx11ContextState
	{ dx11ContextStateFrameBuffer = frameBufferRef
	, dx11ContextStateViewport = viewportRef
	, dx11ContextStateScissor = scissorRef
	, dx11ContextStateVertexBuffers = vertexBuffersVector
	, dx11ContextStateIndexBuffer = indexBufferRef
	, dx11ContextStateUniformBuffers = uniformBuffersVector
	, dx11ContextStateSamplers = samplersVector
	, dx11ContextStateBlendState = blendStateRef
	, dx11ContextStateDepthTestFunc = depthTestFuncRef
	, dx11ContextStateDepthWrite = depthWriteRef
	, dx11ContextStateProgram = programRef
	} = do
	writeIORef frameBufferRef $ Dx11FrameBufferId [] Dx11NullDepthStencilTargetId
	writeIORef viewportRef $ Vec4 0 0 0 0
	writeIORef scissorRef Nothing
	VM.set vertexBuffersVector Dx11NullVertexBufferId
	writeIORef indexBufferRef $ Dx11NullIndexBufferId D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST
	VM.set uniformBuffersVector Dx11NullUniformBufferId
	VM.set samplersVector (Dx11NullTextureId, Dx11NullSamplerStateId)
	writeIORef blendStateRef Dx11NullBlendStateId
	writeIORef depthTestFuncRef DepthTestFuncLess
	writeIORef depthWriteRef True
	writeIORef programRef Dx11NullProgramId

instance Context Dx11Context Dx11Device where
	contextClearColor Dx11Context
		{ dx11ContextInterface = contextInterface
		, dx11ContextDesiredState = Dx11ContextState
			{ dx11ContextStateFrameBuffer = frameBufferRef
			}
		} targetIndex color = do
		Dx11FrameBufferId renderTargets _depthStencilTarget <- readIORef frameBufferRef
		let Dx11RenderTargetId rtvInterface = renderTargets !! targetIndex
		withArray (vecToList color) $ \colorPtr -> do
			m_ID3D11DeviceContext_ClearRenderTargetView contextInterface (pokeCOMObject rtvInterface) colorPtr

	contextClearDepth context depth = do
		dx11ClearDepthStencil context depth 0 (fromEnum D3D11_CLEAR_DEPTH)

	contextClearStencil context stencil = do
		dx11ClearDepthStencil context 0 stencil (fromEnum D3D11_CLEAR_STENCIL)

	contextClearDepthStencil context depth stencil = do
		dx11ClearDepthStencil context depth stencil ((fromEnum D3D11_CLEAR_DEPTH) .|. (fromEnum D3D11_CLEAR_STENCIL))

	contextUploadUniformBuffer context uniformBuffer bytes = do
		case uniformBuffer of
			Dx11UniformBufferId bufferInterface -> dx11UploadBuffer context bufferInterface bytes
			Dx11NullUniformBufferId -> return ()

	contextUploadVertexBuffer context vertexBuffer bytes = do
		case vertexBuffer of
			Dx11VertexBufferId bufferInterface _stride -> dx11UploadBuffer context bufferInterface bytes
			Dx11NullVertexBufferId -> return ()

	contextDraw context@Dx11Context
		{ dx11ContextInterface = contextInterface
		, dx11ContextDesiredState = Dx11ContextState
			{ dx11ContextStateIndexBuffer = indexBufferRef
			}
		} instancesCount indicesCount = do
		dx11UpdateContext context
		indexBuffer <- readIORef indexBufferRef
		case indexBuffer of
			Dx11IndexBufferId _indexBufferInterface _format _primitiveTopology -> do
				if instancesCount > 1 then
					m_ID3D11DeviceContext_DrawIndexedInstanced contextInterface (fromIntegral indicesCount) (fromIntegral instancesCount) 0 0 0
				else
					m_ID3D11DeviceContext_DrawIndexed contextInterface (fromIntegral indicesCount) 0 0
			Dx11NullIndexBufferId _primitiveTopology -> do
				if instancesCount > 1 then
					m_ID3D11DeviceContext_DrawInstanced contextInterface (fromIntegral indicesCount) (fromIntegral instancesCount) 0 0
				else
					m_ID3D11DeviceContext_Draw contextInterface (fromIntegral indicesCount) 0

	-- TODO
	contextPlay = undefined

	contextRender Dx11Context
		{ dx11ContextDesiredState = desiredContextState
		} f = do
		dx11SetDefaultContextState desiredContextState
		f

	contextSetFrameBuffer Dx11Context
		{ dx11ContextDesiredState = Dx11ContextState
			{ dx11ContextStateFrameBuffer = frameBufferRef
			}
		} frameBuffer scope = do
		oldFrameBuffer <- readIORef frameBufferRef
		writeIORef frameBufferRef frameBuffer
		r <- scope
		writeIORef frameBufferRef oldFrameBuffer
		return r

	contextSetViewport Dx11Context
		{ dx11ContextDesiredState = Dx11ContextState
			{ dx11ContextStateViewport = viewportRef
			}
		} viewport scope = do
		oldViewport <- readIORef viewportRef
		writeIORef viewportRef viewport
		r <- scope
		writeIORef viewportRef oldViewport
		return r

	contextGetViewport Dx11Context
		{ dx11ContextDesiredState = Dx11ContextState
			{ dx11ContextStateViewport = viewportRef
			}
		} = readIORef viewportRef

	contextSetScissor Dx11Context
		{ dx11ContextDesiredState = Dx11ContextState
			{ dx11ContextStateScissor = scissorRef
			}
		} scissor scope = do
		oldScissor <- readIORef scissorRef
		writeIORef scissorRef scissor
		r <- scope
		writeIORef scissorRef oldScissor
		return r

	contextGetScissor Dx11Context
		{ dx11ContextDesiredState = Dx11ContextState
			{ dx11ContextStateScissor = scissorRef
			}
		} = readIORef scissorRef

	contextSetVertexBuffer Dx11Context
		{ dx11ContextDesiredState = Dx11ContextState
			{ dx11ContextStateVertexBuffers = vertexBuffersVector
			}
		} i vertexBuffer scope = do
		oldVertexBuffer <- VM.read vertexBuffersVector i
		VM.write vertexBuffersVector i vertexBuffer
		r <- scope
		VM.write vertexBuffersVector i oldVertexBuffer
		return r

	contextSetIndexBuffer Dx11Context
		{ dx11ContextDesiredState = Dx11ContextState
			{ dx11ContextStateIndexBuffer = indexBufferRef
			}
		} indexBuffer scope = do
		oldIndexBuffer <- readIORef indexBufferRef
		writeIORef indexBufferRef indexBuffer
		r <- scope
		writeIORef indexBufferRef oldIndexBuffer
		return r

	contextSetUniformBuffer Dx11Context
		{ dx11ContextDesiredState = Dx11ContextState
			{ dx11ContextStateUniformBuffers = uniformBuffersVector
			}
		} i uniformBuffer scope = do
		oldUniformBuffer <- VM.read uniformBuffersVector i
		VM.write uniformBuffersVector i uniformBuffer
		r <- scope
		VM.write uniformBuffersVector i oldUniformBuffer
		return r

	contextSetSampler Dx11Context
		{ dx11ContextDesiredState = Dx11ContextState
			{ dx11ContextStateSamplers = samplersVector
			}
		} i texture samplerState scope = do
		oldSampler <- VM.read samplersVector i
		VM.write samplersVector i (texture, samplerState)
		r <- scope
		VM.write samplersVector i oldSampler
		return r

	contextSetBlendState Dx11Context
		{ dx11ContextDesiredState = Dx11ContextState
			{ dx11ContextStateBlendState = blendStateRef
			}
		} blendState scope = do
		oldBlendState <- readIORef blendStateRef
		writeIORef blendStateRef blendState
		r <- scope
		writeIORef blendStateRef oldBlendState
		return r

	contextSetDepthTestFunc Dx11Context
		{ dx11ContextDesiredState = Dx11ContextState
			{ dx11ContextStateDepthTestFunc = depthTestFuncRef
			}
		} depthTestFunc scope = do
		oldDepthTestFunc <- readIORef depthTestFuncRef
		writeIORef depthTestFuncRef depthTestFunc
		r <- scope
		writeIORef depthTestFuncRef oldDepthTestFunc
		return r

	contextSetDepthWrite Dx11Context
		{ dx11ContextDesiredState = Dx11ContextState
			{ dx11ContextStateDepthWrite = depthWriteRef
			}
		} depthWrite scope = do
		oldDepthWrite <- readIORef depthWriteRef
		writeIORef depthWriteRef depthWrite
		r <- scope
		writeIORef depthWriteRef oldDepthWrite
		return r

	contextSetProgram Dx11Context
		{ dx11ContextDesiredState = Dx11ContextState
			{ dx11ContextStateProgram = programRef
			}
		} program scope = do
		oldProgram <- readIORef programRef
		writeIORef programRef program
		r <- scope
		writeIORef programRef oldProgram
		return r

-- | DirectX11 DXGI presenter for window.
data Dx11Presenter = Dx11Presenter
	{ dx11PresenterDevice :: Dx11Device
	, dx11PresenterSwapChainInterface :: IDXGISwapChain
	, dx11PresenterNeedDepthStencil :: Bool
	, dx11PresenterWindowSystem :: Win32WindowSystem
	-- | Mutable presenter state. Work only from window thread!
	, dx11PresenterStateRef :: IORef Dx11PresenterState
	}

instance Eq Dx11Presenter where
	Dx11Presenter
		{ dx11PresenterSwapChainInterface = a
		} == Dx11Presenter
		{ dx11PresenterSwapChainInterface = b
		} = a == b

-- | Mutable state of DXGI presenter.
data Dx11PresenterState = Dx11PresenterState
	{ -- | Render target of presenter. May be reset to Nothing if needs an update.
	  dx11PresenterMaybeRTV :: Maybe ID3D11RenderTargetView
		-- | Depth stencil target of presenter. May be reset to Nothing if needs an update (or disabled).
	, dx11PresenterMaybeDSV :: Maybe ID3D11DepthStencilView
	  -- | Current display mode.
	, dx11PresenterMaybeDisplayMode :: Maybe (DisplayModeId DXGISystem)
	, dx11PresenterWidth :: Int
	, dx11PresenterHeight :: Int
	}

instance Presenter Dx11Presenter DXGISystem Dx11Context Dx11Device where
	setPresenterMode presenter@Dx11Presenter
		{ dx11PresenterSwapChainInterface = swapChainInterface
		, dx11PresenterWindowSystem = windowSystem
		, dx11PresenterStateRef = stateRef
		} maybeDisplayMode = describeException ("failed to set DirectX11 presenter mode", maybeDisplayMode) $ invokeWin32WindowSystem windowSystem $ do
		-- get state
		state@Dx11PresenterState
			{ dx11PresenterMaybeDisplayMode = currentMaybeDisplayMode
			, dx11PresenterWidth = width
			, dx11PresenterHeight = height
			} <- readIORef stateRef
		-- resize target
		let desc = getDXGIDisplayModeDesc maybeDisplayMode width height
		with desc $ \descPtr -> do
			hresultCheck =<< m_IDXGISwapChain_ResizeTarget swapChainInterface descPtr
		-- if fullscreen state changed, change
		newState <- if isJust currentMaybeDisplayMode /= isJust maybeDisplayMode then do
			hresultCheck =<< m_IDXGISwapChain_SetFullscreenState swapChainInterface (isJust maybeDisplayMode) nullPtr
			-- if now it's fullscreen, resize buffers
			case maybeDisplayMode of
				Just (DXGIDisplayModeId DXGI_MODE_DESC
					{ f_DXGI_MODE_DESC_Width = modeWidth
					, f_DXGI_MODE_DESC_Height = modeHeight
					}) -> dx11ResizePresenter presenter state (fromIntegral modeWidth) (fromIntegral modeHeight)
				_ -> return state
		else return state
		-- set new mode
		writeIORef stateRef newState
			{ dx11PresenterMaybeDisplayMode = maybeDisplayMode
			}

	presenterRender Dx11Presenter
		{ dx11PresenterDevice = Dx11Device
			{ dx11DeviceInterface = deviceInterface
			}
		, dx11PresenterSwapChainInterface = swapChainInterface
		, dx11PresenterNeedDepthStencil = needDepthStencil
		, dx11PresenterWindowSystem = windowSystem
		, dx11PresenterStateRef = stateRef
		} Dx11Context
		{ dx11ContextDesiredState = Dx11ContextState
			{ dx11ContextStateFrameBuffer = frameBufferRef
			, dx11ContextStateViewport = viewportRef
			}
		} f = do
		-- sync with window
		invokeWin32WindowSystem windowSystem $ do
			-- get RTV for backbuffer (create if needed)
			Dx11PresenterState
				{ dx11PresenterMaybeRTV = maybeRTV
				, dx11PresenterMaybeDSV = maybeDSV
				, dx11PresenterWidth = width
				, dx11PresenterHeight = height
				} <- readIORef stateRef
			rtv <- case maybeRTV of
				Just rtv -> return rtv
				Nothing -> do
					-- create new RTV
					backBufferTextureInterfacePtr <- with (getIID (undefined :: ID3D11Texture2D)) $ \iidPtr -> do
						liftM castPtr $ createCOMValueViaPtr $ m_IDXGISwapChain_GetBuffer swapChainInterface 0 iidPtr
					rtv <- createCOMObjectViaPtr $ m_ID3D11Device_CreateRenderTargetView deviceInterface backBufferTextureInterfacePtr nullPtr
					_ <- m_IUnknown_Release =<< peekCOMObject backBufferTextureInterfacePtr
					-- save it in state
					modifyIORef stateRef $ \s -> s
						{ dx11PresenterMaybeRTV = Just rtv
						}
					return rtv
			depthStencilTarget <- case maybeDSV of
				Just dsv -> return $ Dx11DepthStencilTargetId dsv
				Nothing -> do
					if needDepthStencil then do
						-- create new DSV
						-- resource desc
						let desc = D3D11_TEXTURE2D_DESC
							{ f_D3D11_TEXTURE2D_DESC_Width = fromIntegral width
							, f_D3D11_TEXTURE2D_DESC_Height = fromIntegral height
							, f_D3D11_TEXTURE2D_DESC_MipLevels = 1
							, f_D3D11_TEXTURE2D_DESC_ArraySize = 1
							, f_D3D11_TEXTURE2D_DESC_Format = DXGI_FORMAT_R24G8_TYPELESS
							, f_D3D11_TEXTURE2D_DESC_SampleDesc = DXGI_SAMPLE_DESC
								{ f_DXGI_SAMPLE_DESC_Count = 1
								, f_DXGI_SAMPLE_DESC_Quality = 0
								}
							, f_D3D11_TEXTURE2D_DESC_Usage = D3D11_USAGE_DEFAULT
							, f_D3D11_TEXTURE2D_DESC_BindFlags = fromIntegral $ fromEnum D3D11_BIND_DEPTH_STENCIL
							, f_D3D11_TEXTURE2D_DESC_CPUAccessFlags = 0
							, f_D3D11_TEXTURE2D_DESC_MiscFlags = 0
							}
						-- create resource
						resourceInterface <- with desc $ \descPtr -> do
							liftM com_get_ID3D11Resource $ createCOMObjectViaPtr $ m_ID3D11Device_CreateTexture2D deviceInterface descPtr nullPtr

						-- DSV desc
						let dsvDesc = D3D11_DEPTH_STENCIL_VIEW_DESC_Texture2D
							{ f_D3D11_DEPTH_STENCIL_VIEW_DESC_Format = DXGI_FORMAT_D24_UNORM_S8_UINT
							, f_D3D11_DEPTH_STENCIL_VIEW_DESC_ViewDimension = D3D11_DSV_DIMENSION_TEXTURE2D
							, f_D3D11_DEPTH_STENCIL_VIEW_DESC_Flags = 0
							, f_D3D11_DEPTH_STENCIL_VIEW_DESC_Texture2D = D3D11_TEX2D_DSV
								{ f_D3D11_TEX2D_DSV_MipSlice = 0
								}
							}
						-- create depth stencil view
						dsvInterface <- with dsvDesc $ \dsvDescPtr -> do
							createCOMObjectViaPtr $ m_ID3D11Device_CreateDepthStencilView deviceInterface (pokeCOMObject resourceInterface) dsvDescPtr

						-- release resource
						_ <- m_IUnknown_Release resourceInterface

						-- save it in state
						modifyIORef stateRef $ \s -> s
							{ dx11PresenterMaybeDSV = Just dsvInterface
							}
						return $ Dx11DepthStencilTargetId dsvInterface
					else return Dx11NullDepthStencilTargetId

			-- set context state
			writeIORef frameBufferRef $ Dx11FrameBufferId [Dx11RenderTargetId rtv] depthStencilTarget
			writeIORef viewportRef $ Vec4 0 0 width height

			-- perform render
			r <- f

			-- present
			hresultCheck =<< m_IDXGISwapChain_Present swapChainInterface 1 0

			return r

dx11ResizePresenter :: Dx11Presenter -> Dx11PresenterState -> Int -> Int -> IO Dx11PresenterState
dx11ResizePresenter Dx11Presenter
	{ dx11PresenterSwapChainInterface = swapChainInterface
	} state@Dx11PresenterState
	{ dx11PresenterMaybeRTV = maybePreviousRTV
	, dx11PresenterMaybeDSV = maybePreviousDSV
	} width height = do
	-- release previous RTV if any
	case maybePreviousRTV of
		Just previousRTV -> do
			_ <- m_IUnknown_Release previousRTV
			return ()
		Nothing -> return ()
	-- release previous DSV if any
	case maybePreviousDSV of
		Just previousDSV -> do
			_ <- m_IUnknown_Release previousDSV
			return ()
		Nothing -> return ()
	-- resize buffers
	hresultCheck =<< m_IDXGISwapChain_ResizeBuffers swapChainInterface 0
		(fromIntegral width) (fromIntegral height) (wrapEnum DXGI_FORMAT_UNKNOWN) (fromIntegral $ fromEnum DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH)
	-- remember size
	return state
		{ dx11PresenterMaybeRTV = Nothing
		, dx11PresenterMaybeDSV = Nothing
		, dx11PresenterWidth = width
		, dx11PresenterHeight = height
		}

dx11CreatePresenter :: Dx11Device -> Win32Window -> Maybe (DisplayModeId DXGISystem) -> Bool -> IO (Dx11Presenter, IO ())
dx11CreatePresenter device@Dx11Device
	{ dx11DeviceSystem = DXGISystem
		{ dxgiSystemFactory = factoryInterface
		}
	, dx11DeviceInterface = deviceInterface
	} window@Win32Window
	{ wWindowSystem = windowSystem
	, wHandle = hwnd
	} maybeDisplayMode needDepthStencil = do

	-- window client size
	(initialWidth, initialHeight) <- getWindowClientSize window

	-- swap chain desc
	let desc = DXGI_SWAP_CHAIN_DESC
		{ f_DXGI_SWAP_CHAIN_DESC_BufferDesc = getDXGIDisplayModeDesc maybeDisplayMode initialWidth initialHeight
		, f_DXGI_SWAP_CHAIN_DESC_SampleDesc = DXGI_SAMPLE_DESC
			{ f_DXGI_SAMPLE_DESC_Count = 1
			, f_DXGI_SAMPLE_DESC_Quality = 0
			}
		, f_DXGI_SWAP_CHAIN_DESC_BufferUsage = fromIntegral $ (fromEnum DXGI_USAGE_BACK_BUFFER) .|. (fromEnum DXGI_USAGE_RENDER_TARGET_OUTPUT)
		, f_DXGI_SWAP_CHAIN_DESC_BufferCount = 2
		, f_DXGI_SWAP_CHAIN_DESC_OutputWindow = hwnd
		, f_DXGI_SWAP_CHAIN_DESC_Windowed = True
		, f_DXGI_SWAP_CHAIN_DESC_SwapEffect = DXGI_SWAP_EFFECT_DISCARD
		, f_DXGI_SWAP_CHAIN_DESC_Flags = fromIntegral $ fromEnum DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH
		}

	-- create swap chain
	(swapChainInterface, releaseSwapChainInterface) <- allocateCOMObject $ do
		with desc $ \descPtr -> do
			createCOMObjectViaPtr $ m_IDXGIFactory_CreateSwapChain factoryInterface (pokeCOMObject $ com_get_IUnknown deviceInterface) descPtr

	-- presenter state ref
	stateRef <- newIORef Dx11PresenterState
		{ dx11PresenterMaybeRTV = Nothing
		, dx11PresenterMaybeDSV = Nothing
		, dx11PresenterMaybeDisplayMode = Nothing
		, dx11PresenterWidth = initialWidth
		, dx11PresenterHeight = initialHeight
		}

	-- create presenter
	let presenter = Dx11Presenter
		{ dx11PresenterDevice = device
		, dx11PresenterSwapChainInterface = swapChainInterface
		, dx11PresenterNeedDepthStencil = needDepthStencil
		, dx11PresenterWindowSystem = windowSystem
		, dx11PresenterStateRef = stateRef
		}

	-- create ioref for window callback
	presenterValidRef <- newIORef True

	-- set window callback
	addWin32WindowCallback window $ \msg _wParam lParam -> do
		case msg of
			0x0005 {- WM_SIZE -} -> do
				presenterValid <- readIORef presenterValidRef
				if presenterValid then do
					state <- readIORef stateRef
					writeIORef stateRef =<< dx11ResizePresenter presenter state (fromIntegral $ loWord lParam) (fromIntegral $ hiWord lParam)
				else return ()
			_ -> return ()

	-- set mode
	setPresenterMode presenter maybeDisplayMode

	let destroy = do
		invokeWin32WindowSystem windowSystem $ do
			-- set that presenter is invalid
			writeIORef presenterValidRef False
			-- free RTV if needed
			Dx11PresenterState
				{ dx11PresenterMaybeRTV = maybeRTV
				, dx11PresenterMaybeDSV = maybeDSV
				} <- readIORef stateRef
			case maybeRTV of
				Just rtv -> do
					_ <- m_IUnknown_Release rtv
					modifyIORef stateRef $ \s -> s
						{ dx11PresenterMaybeRTV = Nothing
						}
				Nothing -> return ()
			case maybeDSV of
				Just dsv -> do
					_ <- m_IUnknown_Release dsv
					modifyIORef stateRef $ \s -> s
						{ dx11PresenterMaybeDSV = Nothing
						}
				Nothing -> return ()
		releaseSwapChainInterface

	return (presenter, destroy)

-- | Helper method to clear depth and/or stencil.
dx11ClearDepthStencil :: Dx11Context -> Float -> Int -> Int -> IO ()
dx11ClearDepthStencil Dx11Context
	{ dx11ContextInterface = contextInterface
	, dx11ContextDesiredState = Dx11ContextState
		{ dx11ContextStateFrameBuffer = frameBufferRef
		}
	} depth stencil flags = do
	Dx11FrameBufferId _renderTargets depthStencilTarget <- readIORef frameBufferRef
	case depthStencilTarget of
		Dx11DepthStencilTargetId dsvInterface -> do
			m_ID3D11DeviceContext_ClearDepthStencilView contextInterface (pokeCOMObject dsvInterface) (fromIntegral flags) depth (fromIntegral stencil)
		Dx11NullDepthStencilTargetId -> return ()

-- | Helper method to upload bytes to buffer.
dx11UploadBuffer :: ID3D11Resource_Class b => Dx11Context -> b -> B.ByteString -> IO ()
dx11UploadBuffer Dx11Context
	{ dx11ContextInterface = contextInterface
	} resource bytes = do
	let resourceInterfacePtr = pokeCOMObject $ com_get_ID3D11Resource resource
	D3D11_MAPPED_SUBRESOURCE
		{ f_D3D11_MAPPED_SUBRESOURCE_pData = bufferPtr
		} <- createCOMValueViaPtr $ m_ID3D11DeviceContext_Map contextInterface resourceInterfacePtr 0 (wrapEnum D3D11_MAP_WRITE_DISCARD) 0
	B.unsafeUseAsCStringLen bytes $ \(bytesPtr, bytesLen) -> copyBytes bufferPtr (castPtr bytesPtr) bytesLen
	m_ID3D11DeviceContext_Unmap contextInterface resourceInterfacePtr 0

dx11DepthTestFunc :: DepthTestFunc -> D3D11_COMPARISON_FUNC
dx11DepthTestFunc depthTestFunc = case depthTestFunc of
	DepthTestFuncNever -> D3D11_COMPARISON_NEVER
	DepthTestFuncLess -> D3D11_COMPARISON_LESS
	DepthTestFuncLessOrEqual -> D3D11_COMPARISON_LESS_EQUAL
	DepthTestFuncEqual -> D3D11_COMPARISON_EQUAL
	DepthTestFuncNonEqual -> D3D11_COMPARISON_NOT_EQUAL
	DepthTestFuncGreaterOrEqual -> D3D11_COMPARISON_GREATER_EQUAL
	DepthTestFuncGreater -> D3D11_COMPARISON_GREATER
	DepthTestFuncAlways -> D3D11_COMPARISON_ALWAYS

dx11CalcDepthStencilStateCode :: D3D11_COMPARISON_FUNC -> Bool -> Int
dx11CalcDepthStencilStateCode comparisonFunc depthWrite = (fromEnum comparisonFunc) .|. (if depthWrite then 8 else 0)

dx11GetDepthStencilState :: Dx11Context -> D3D11_COMPARISON_FUNC -> Bool -> Int -> IO ID3D11DepthStencilState
dx11GetDepthStencilState Dx11Context
	{ dx11ContextDevice = Dx11Device
		{ dx11DeviceInterface = deviceInterface
		}
	, dx11ContextDepthStencilStateCache = depthStencilStateCacheRef
	} comparisonFunc depthWrite code = do
	depthStencilStateCache <- readIORef depthStencilStateCacheRef
	case HM.lookup code depthStencilStateCache of
		Just depthStencilInterface -> return depthStencilInterface
		Nothing -> do
			let stencilOpDesc = D3D11_DEPTH_STENCILOP_DESC
				{ f_D3D11_DEPTH_STENCILOP_DESC_StencilFailOp = D3D11_STENCIL_OP_KEEP
				, f_D3D11_DEPTH_STENCILOP_DESC_StencilDepthFailOp = D3D11_STENCIL_OP_KEEP
				, f_D3D11_DEPTH_STENCILOP_DESC_StencilPassOp = D3D11_STENCIL_OP_KEEP
				, f_D3D11_DEPTH_STENCILOP_DESC_StencilFunc = D3D11_COMPARISON_ALWAYS
				}
			let desc = D3D11_DEPTH_STENCIL_DESC
				{ f_D3D11_DEPTH_STENCIL_DESC_DepthEnable = comparisonFunc /= D3D11_COMPARISON_ALWAYS || depthWrite
				, f_D3D11_DEPTH_STENCIL_DESC_DepthWriteMask = if depthWrite then D3D11_DEPTH_WRITE_MASK_ALL else D3D11_DEPTH_WRITE_MASK_ZERO
				, f_D3D11_DEPTH_STENCIL_DESC_DepthFunc = comparisonFunc
				, f_D3D11_DEPTH_STENCIL_DESC_StencilEnable = False
				, f_D3D11_DEPTH_STENCIL_DESC_StencilReadMask = 0
				, f_D3D11_DEPTH_STENCIL_DESC_StencilWriteMask = 0
				, f_D3D11_DEPTH_STENCIL_DESC_FrontFace = stencilOpDesc
				, f_D3D11_DEPTH_STENCIL_DESC_BackFace = stencilOpDesc
				}
			depthStencilInterface <- with desc $ \descPtr -> do
				createCOMObjectViaPtr $ m_ID3D11Device_CreateDepthStencilState deviceInterface descPtr
			writeIORef depthStencilStateCacheRef $ HM.insert code depthStencilInterface depthStencilStateCache
			return depthStencilInterface

-- | Update context.
dx11UpdateContext :: Dx11Context -> IO ()
dx11UpdateContext context@Dx11Context
	{ dx11ContextInterface = contextInterface
	, dx11ContextActualState = Dx11ContextState
		{ dx11ContextStateFrameBuffer = actualFrameBufferRef
		, dx11ContextStateViewport = actualViewportRef
		, dx11ContextStateScissor = actualScissorRef
		, dx11ContextStateVertexBuffers = actualVertexBuffersVector
		, dx11ContextStateIndexBuffer = actualIndexBufferRef
		, dx11ContextStateUniformBuffers = actualUniformBuffersVector
		, dx11ContextStateSamplers = actualSamplersVector
		, dx11ContextStateBlendState = actualBlendStateRef
		, dx11ContextStateDepthTestFunc = actualDepthTestFuncRef
		, dx11ContextStateDepthWrite = actualDepthWriteRef
		, dx11ContextStateProgram = actualProgramRef
		}
	, dx11ContextDesiredState = Dx11ContextState
		{ dx11ContextStateFrameBuffer = desiredFrameBufferRef
		, dx11ContextStateViewport = desiredViewportRef
		, dx11ContextStateScissor = desiredScissorRef
		, dx11ContextStateVertexBuffers = desiredVertexBuffersVector
		, dx11ContextStateIndexBuffer = desiredIndexBufferRef
		, dx11ContextStateUniformBuffers = desiredUniformBuffersVector
		, dx11ContextStateSamplers = desiredSamplersVector
		, dx11ContextStateBlendState = desiredBlendStateRef
		, dx11ContextStateDepthTestFunc = desiredDepthTestFuncRef
		, dx11ContextStateDepthWrite = desiredDepthWriteRef
		, dx11ContextStateProgram = desiredProgramRef
		}
	} = do

	let
		refSetup :: Eq a => IORef a -> IORef a -> (a -> IO ()) -> IO ()
		refSetup actualRef desiredRef setup = do
			actual <- readIORef actualRef
			desired <- readIORef desiredRef
			if actual /= desired then do
				setup desired
				writeIORef actualRef desired
			else return ()

	let
		vectorSetup :: Eq a => VM.IOVector a -> VM.IOVector a -> (V.Vector a -> IO ()) -> IO ()
		vectorSetup actualVector desiredVector setup = do
			let len = VM.length actualVector
			let f i = do
				if i < len then do
					actual <- VM.unsafeRead actualVector i
					desired <- VM.unsafeRead desiredVector i
					if actual /= desired then do
						freezedDesiredVector <- V.freeze desiredVector
						setup freezedDesiredVector
						V.copy actualVector freezedDesiredVector
					else f $ i + 1
				else return ()
			f 0

	-- framebuffer
	refSetup actualFrameBufferRef desiredFrameBufferRef $ \desiredFrameBuffer -> do
		let Dx11FrameBufferId renderTargets depthStencilTarget = desiredFrameBuffer
		let renderTargetsInterfaces = [pokeCOMObject rtv | Dx11RenderTargetId rtv <- renderTargets]
		let depthStencilInterface = case depthStencilTarget of
			Dx11DepthStencilTargetId interface -> pokeCOMObject interface
			Dx11NullDepthStencilTargetId -> nullPtr
		withArray renderTargetsInterfaces $ \renderTargetsInterfacesPtr -> do
			m_ID3D11DeviceContext_OMSetRenderTargets contextInterface (fromIntegral $ length renderTargetsInterfaces) renderTargetsInterfacesPtr depthStencilInterface

	-- viewport
	refSetup actualViewportRef desiredViewportRef $ \(Vec4 left top right bottom) -> do
		-- set new viewport
		let viewport = D3D11_VIEWPORT
			{ f_D3D11_VIEWPORT_TopLeftX = fromIntegral left
			, f_D3D11_VIEWPORT_TopLeftY = fromIntegral top
			, f_D3D11_VIEWPORT_Width = fromIntegral (right - left)
			, f_D3D11_VIEWPORT_Height = fromIntegral (bottom - top)
			, f_D3D11_VIEWPORT_MinDepth = 0
			, f_D3D11_VIEWPORT_MaxDepth = 1
			}
		with viewport $ \viewportPtr -> do
			m_ID3D11DeviceContext_RSSetViewports contextInterface 1 viewportPtr

	-- scissor
	refSetup actualScissorRef desiredScissorRef $ \maybeScissor -> do
		case maybeScissor of
			Just (Vec4 left top right bottom) -> with RECT
				{ f_RECT_left = fromIntegral left
				, f_RECT_top = fromIntegral top
				, f_RECT_right = fromIntegral right
				, f_RECT_bottom = fromIntegral bottom
				} $ m_ID3D11DeviceContext_RSSetScissorRects contextInterface 1
			Nothing -> m_ID3D11DeviceContext_RSSetScissorRects contextInterface 0 nullPtr

	-- vertex buffers
	vectorSetup actualVertexBuffersVector desiredVertexBuffersVector $ \desiredVertexBuffers -> do
		let buffersCount = V.length desiredVertexBuffers
		allocaArray buffersCount $ \buffersInterfacesPtr -> allocaArray buffersCount $ \stridesPtr -> allocaArray buffersCount $ \offsetsPtr -> do
			(flip V.imapM_) desiredVertexBuffers $ \i vertexBuffer -> do
				let (bufferInterfacePtr, stride) = case vertexBuffer of
					Dx11VertexBufferId bufferInterface stride' -> (pokeCOMObject bufferInterface, stride')
					Dx11NullVertexBufferId -> (nullPtr, 0)
				pokeElemOff buffersInterfacesPtr i bufferInterfacePtr
				pokeElemOff stridesPtr i $ fromIntegral stride
				pokeElemOff offsetsPtr i 0
			m_ID3D11DeviceContext_IASetVertexBuffers contextInterface 0 (fromIntegral buffersCount) buffersInterfacesPtr stridesPtr offsetsPtr

	-- index buffer
	refSetup actualIndexBufferRef desiredIndexBufferRef $ \desiredIndexBuffer -> do
		let (indexBufferInterfacePtr, format, primitiveTopology) = case desiredIndexBuffer of
			Dx11IndexBufferId bufferInterface format' primitiveTopology' -> (pokeCOMObject bufferInterface, format', primitiveTopology')
			Dx11NullIndexBufferId primitiveTopology' -> (nullPtr, DXGI_FORMAT_UNKNOWN, primitiveTopology')
		m_ID3D11DeviceContext_IASetIndexBuffer contextInterface indexBufferInterfacePtr (wrapEnum format) 0
		m_ID3D11DeviceContext_IASetPrimitiveTopology contextInterface (wrapEnum primitiveTopology)

	-- uniform buffers
	vectorSetup actualUniformBuffersVector desiredUniformBuffersVector $ \desiredUniformBuffers -> do
		let buffersCount = V.length desiredUniformBuffers
		allocaArray buffersCount $ \buffersInterfacesPtr -> do
			(flip V.imapM_) desiredUniformBuffers $ \i uniformBuffer -> do
				pokeElemOff buffersInterfacesPtr i $ case uniformBuffer of
					Dx11UniformBufferId bufferInterface -> pokeCOMObject bufferInterface
					Dx11NullUniformBufferId -> nullPtr
			m_ID3D11DeviceContext_VSSetConstantBuffers contextInterface 0 (fromIntegral buffersCount) buffersInterfacesPtr
			m_ID3D11DeviceContext_PSSetConstantBuffers contextInterface 0 (fromIntegral buffersCount) buffersInterfacesPtr

	-- samplers
	vectorSetup actualSamplersVector desiredSamplersVector $ \desiredSamplers -> do
		let samplersCount = V.length desiredSamplers
		allocaArray samplersCount $ \srvInterfacesPtr -> do
			allocaArray samplersCount $ \ssInterfacesPtr -> do
				(flip V.imapM_) desiredSamplers $ \i (texture, samplerState) -> do
					let (srvInterfacePtr, ssInterfacePtr) = case texture of
						Dx11TextureId srvInterface textureSamplerState -> case samplerState of
							Dx11SamplerStateId ssInterface -> (pokeCOMObject srvInterface, pokeCOMObject ssInterface)
							Dx11NullSamplerStateId -> case textureSamplerState of
								Dx11SamplerStateId ssInterface -> (pokeCOMObject srvInterface, pokeCOMObject ssInterface)
								Dx11NullSamplerStateId -> (pokeCOMObject srvInterface, nullPtr)
						Dx11NullTextureId -> (nullPtr, nullPtr)
					pokeElemOff srvInterfacesPtr i srvInterfacePtr
					pokeElemOff ssInterfacesPtr i ssInterfacePtr
				m_ID3D11DeviceContext_VSSetShaderResources contextInterface 0 (fromIntegral samplersCount) srvInterfacesPtr
				m_ID3D11DeviceContext_PSSetShaderResources contextInterface 0 (fromIntegral samplersCount) srvInterfacesPtr
				m_ID3D11DeviceContext_VSSetSamplers contextInterface 0 (fromIntegral samplersCount) ssInterfacesPtr
				m_ID3D11DeviceContext_PSSetSamplers contextInterface 0 (fromIntegral samplersCount) ssInterfacesPtr

	-- blend state
	refSetup actualBlendStateRef desiredBlendStateRef $ \desiredBlendState -> do
		let blendStateInterfacePtr = case desiredBlendState of
			Dx11BlendStateId blendStateInterface -> pokeCOMObject blendStateInterface
			Dx11NullBlendStateId -> nullPtr
		m_ID3D11DeviceContext_OMSetBlendState contextInterface blendStateInterfacePtr nullPtr 0xffffffff

	-- depth-stencil state
	do
		actualDepthTestFunc <- readIORef actualDepthTestFuncRef
		actualDepthWrite <- readIORef actualDepthWriteRef
		desiredDepthTestFunc <- readIORef desiredDepthTestFuncRef
		desiredDepthWrite <- readIORef desiredDepthWriteRef
		if actualDepthTestFunc /= desiredDepthTestFunc || actualDepthWrite /= desiredDepthWrite then do
			let comparisonFunc = dx11DepthTestFunc desiredDepthTestFunc
			let code = dx11CalcDepthStencilStateCode comparisonFunc desiredDepthWrite
			depthStencilInterface <- dx11GetDepthStencilState context comparisonFunc desiredDepthWrite code
			m_ID3D11DeviceContext_OMSetDepthStencilState contextInterface (pokeCOMObject depthStencilInterface) 0
			writeIORef actualDepthTestFuncRef desiredDepthTestFunc
			writeIORef actualDepthWriteRef desiredDepthWrite
		else return ()

	-- program (shaders, input layout)
	refSetup actualProgramRef desiredProgramRef $ \desiredProgram -> do
		(inputLayoutInterfacePtr, vertexShaderInterfacePtr, pixelShaderInterfacePtr) <- case desiredProgram of
			Dx11VertexPixelProgramId inputLayoutInterface vertexShaderInterface pixelShaderInterface -> return
				( pokeCOMObject inputLayoutInterface
				, pokeCOMObject vertexShaderInterface
				, pokeCOMObject pixelShaderInterface
				)
			Dx11NullProgramId -> return
				( nullPtr
				, nullPtr
				, nullPtr
				)
		m_ID3D11DeviceContext_IASetInputLayout contextInterface inputLayoutInterfacePtr
		m_ID3D11DeviceContext_VSSetShader contextInterface vertexShaderInterfacePtr nullPtr 0
		m_ID3D11DeviceContext_PSSetShader contextInterface pixelShaderInterfacePtr nullPtr 0
