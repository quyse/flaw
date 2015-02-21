{-|
Module: Flaw.Graphics.DirectX11.Internal
Description: Internals of graphics implementation for DirectX 11.
License: MIT
-}

{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}

module Flaw.Graphics.DirectX11.Internal
	( Dx11Device(..)
	, createDx11Device
	) where

import qualified Control.Exception.Lifted as Lifted
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.IORef
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Flaw.Exception
import Flaw.FFI
import Flaw.FFI.COM
import Flaw.Graphics
import Flaw.Graphics.DirectX11.FFI
import Flaw.Graphics.DirectX11.HLSL
import Flaw.Graphics.DXGI.FFI
import Flaw.Graphics.DXGI.Internal
import Flaw.Graphics.Internal
import Flaw.Graphics.Program.Internal
import Flaw.Graphics.Sampler
import Flaw.Graphics.Texture
import Flaw.Math

-- | DirectX11 graphics device.
data Dx11Device = Dx11Device
	{
	-- | Main device interface.
	  dx11DeviceInterface :: ID3D11Device
	-- | Device's immediate context.
	, dx11DeviceImmediateContext :: ID3D11DeviceContext
	}

instance Device Dx11Device where
	type DeferredContext Dx11Device = Dx11Context
	type DeviceProgramGenerator Dx11Device = HlslGenerator
	data TextureId Dx11Device
		= Dx11TextureId ID3D11ShaderResourceView
		| Dx11NullTextureId
		deriving Eq
	newtype SamplerStateId Dx11Device
		= Dx11SamplerStateId ID3D11SamplerState
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
		= Dx11IndexBufferId ID3D11Buffer
		| Dx11NullIndexBufferId
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
	nullDepthStencilTarget = Dx11NullDepthStencilTargetId
	nullIndexBuffer = Dx11NullIndexBufferId
	nullUniformBuffer = Dx11NullUniformBufferId

	createDeferredContext Dx11Device
		{ dx11DeviceInterface = deviceInterface
		} = describeException "failed to create DirectX11 deferred context" $ do
		(releaseKey, contextInterface) <- allocateCOMObject $ createCOMObjectViaPtr $ m_ID3D11Device_CreateDeferredContext deviceInterface 0
		contextState <- liftIO $ newIORef dx11DefaultContextState
		return (releaseKey, Dx11Context
			{ dx11ContextInterface = contextInterface
			, dx11ContextState = contextState
			})

	createStaticTexture Dx11Device
		{ dx11DeviceInterface = deviceInterface
		} textureInfo@TextureInfo
		{ textureWidth = width
		, textureHeight = height
		, textureDepth = depth
		, textureMips = mips
		, textureFormat = format
		, textureCount = count
		} bytes = describeException ("failed to create DirectX11 static texture", textureInfo) $ do
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
				if count > 0 then Lifted.throwIO $ DescribeFirstException "array of 3D textures is not supported"
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
		(resourceReleaseKey, resourceInterface) <- allocateCOMObject $ BS.unsafeUseAsCString bytes createResource
		-- create ID3D11ShaderResourceView
		(srvReleaseKey, srvInterface) <- allocateCOMObject $ with srvDesc $ \srvDescPtr -> do
			createCOMObjectViaPtr $ m_ID3D11Device_CreateShaderResourceView deviceInterface (pokeCOMObject resourceInterface) srvDescPtr
		-- release resource interface
		release resourceReleaseKey

		return (srvReleaseKey, Dx11TextureId srvInterface)

	createSamplerState Dx11Device
		{ dx11DeviceInterface = deviceInterface
		} samplerStateInfo@SamplerStateInfo
		{ samplerMinFilter = minFilter
		, samplerMipFilter = mipFilter
		, samplerMagFilter = magFilter
		, samplerWrapU = wrapU
		, samplerWrapV = wrapV
		, samplerWrapW = wrapW
		, samplerMinLOD = minLOD
		, samplerMaxLOD = maxLOD
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
			, f_D3D11_SAMPLER_DESC_MinLOD = minLOD
			, f_D3D11_SAMPLER_DESC_MaxLOD = maxLOD
			}

		-- create
		(releaseKey, ssInterface) <- allocateCOMObject $ with desc $ \descPtr -> do
			createCOMObjectViaPtr $ m_ID3D11Device_CreateSamplerState deviceInterface descPtr

		return (releaseKey, Dx11SamplerStateId ssInterface)

	createReadableRenderTarget Dx11Device
		{ dx11DeviceInterface = deviceInterface
		} width height format = describeException ("failed to create DirectX11 readable render target", width, height, format) $ do

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
		(resourceReleaseKey, resourceInterface) <- allocateCOMObject $ with desc $ \descPtr -> do
			liftM com_get_ID3D11Resource $ createCOMObjectViaPtr $ m_ID3D11Device_CreateTexture2D deviceInterface descPtr nullPtr

		-- create render target view
		(rtvReleaseKey, rtvInterface) <- allocateCOMObject $ createCOMObjectViaPtr $ m_ID3D11Device_CreateRenderTargetView deviceInterface (pokeCOMObject resourceInterface) nullPtr

		-- create shader resource view
		(srvReleaseKey, srvInterface) <- allocateCOMObject $ createCOMObjectViaPtr $ m_ID3D11Device_CreateShaderResourceView deviceInterface (pokeCOMObject resourceInterface) nullPtr

		-- release resource interface
		release resourceReleaseKey

		-- make combine release key
		releaseKey <- register $ do
			release rtvReleaseKey
			release srvReleaseKey

		return (releaseKey, Dx11RenderTargetId rtvInterface, Dx11TextureId srvInterface)

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
			, f_D3D11_TEXTURE2D_DESC_BindFlags = fromIntegral $ fromEnum D3D11_BIND_RENDER_TARGET
			, f_D3D11_TEXTURE2D_DESC_CPUAccessFlags = 0
			, f_D3D11_TEXTURE2D_DESC_MiscFlags = 0
			}
		-- create resource
		(resourceReleaseKey, resourceInterface) <- allocateCOMObject $ with desc $ \descPtr -> do
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
		(dsvReleaseKey, dsvInterface) <- allocateCOMObject $ with dsvDesc $ \dsvDescPtr -> do
			createCOMObjectViaPtr $ m_ID3D11Device_CreateDepthStencilView deviceInterface (pokeCOMObject resourceInterface) dsvDescPtr

		-- release resource interface
		release resourceReleaseKey

		return (dsvReleaseKey, Dx11DepthStencilTargetId dsvInterface)

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
			, f_D3D11_TEXTURE2D_DESC_BindFlags = fromIntegral ((fromEnum D3D11_BIND_RENDER_TARGET) .|. (fromEnum D3D11_BIND_SHADER_RESOURCE))
			, f_D3D11_TEXTURE2D_DESC_CPUAccessFlags = 0
			, f_D3D11_TEXTURE2D_DESC_MiscFlags = 0
			}
		-- create resource
		(resourceReleaseKey, resourceInterface) <- allocateCOMObject $ with desc $ \descPtr -> do
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
		(dsvReleaseKey, dsvInterface) <- allocateCOMObject $ with dsvDesc $ \dsvDescPtr -> do
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
		(srvReleaseKey, srvInterface) <- allocateCOMObject $ with srvDesc $ \srvDescPtr -> do
			createCOMObjectViaPtr $ m_ID3D11Device_CreateShaderResourceView deviceInterface (pokeCOMObject resourceInterface) srvDescPtr

		-- release resource interface
		release resourceReleaseKey

		-- make combined release key
		releaseKey <- register $ do
			release dsvReleaseKey
			release srvReleaseKey

		return (releaseKey, Dx11DepthStencilTargetId dsvInterface, Dx11TextureId srvInterface)

	createFrameBuffer _device renderTargets depthStencilTarget = do
		releaseKey <- register $ return ()
		return (releaseKey, Dx11FrameBufferId renderTargets depthStencilTarget)

	createStaticVertexBuffer Dx11Device
		{ dx11DeviceInterface = deviceInterface
		} bytes stride = describeException "failed to create DirectX11 vertex buffer" $ do
		-- desc
		let desc = D3D11_BUFFER_DESC
			{ f_D3D11_BUFFER_DESC_ByteWidth = fromIntegral $ BS.length bytes
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
		(releaseKey, bufferInterface) <- allocateCOMObject $ with desc $ \descPtr -> do
			BS.unsafeUseAsCString bytes $ \bytesPtr -> do
				with (subresourceData $ castPtr bytesPtr) $ \subresourceDataPtr -> do
					createCOMObjectViaPtr $ m_ID3D11Device_CreateBuffer deviceInterface descPtr subresourceDataPtr

		return (releaseKey, Dx11VertexBufferId bufferInterface stride)

	createStaticIndexBuffer Dx11Device
		{ dx11DeviceInterface = deviceInterface
		} bytes = describeException "failed to create DirectX11 index buffer" $ do
		-- desc
		let desc = D3D11_BUFFER_DESC
			{ f_D3D11_BUFFER_DESC_ByteWidth = fromIntegral $ BS.length bytes
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
		(releaseKey, bufferInterface) <- allocateCOMObject $ with desc $ \descPtr -> do
			BS.unsafeUseAsCString bytes $ \bytesPtr -> do
				with (subresourceData $ castPtr bytesPtr) $ \subresourceDataPtr -> do
					createCOMObjectViaPtr $ m_ID3D11Device_CreateBuffer deviceInterface descPtr subresourceDataPtr

		return (releaseKey, Dx11IndexBufferId bufferInterface)

	createProgram Dx11Device
		{ dx11DeviceInterface = deviceInterface
		} program = describeException "failed to create DirectX11 program" $ do

		-- function to create input layout
		let createInputLayout attributes vertexShaderByteCode = allocateCOMObject $ BS.unsafeUseAsCStringLen vertexShaderByteCode $ \(byteCodePtr, byteCodeSize) -> let
			inputElementDescs (HlslAttribute
				{ hlslAttributeName = name
				, hlslAttributeSemantic = semantic
				, hlslAttributeSlot = slot
				, hlslAttributeOffset = offset
				, hlslAttributeDivisor = divisor
				, hlslAttributeAttributeType = atype
				} : restAttributes) descs = do
				BS.unsafeUseAsCString (T.encodeUtf8 semantic) $ \semanticPtr -> do
					let format = convertFormat atype
					if format == DXGI_FORMAT_UNKNOWN then fail $ "wrong attribute format for " ++ T.unpack name
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
				ProgramAttributeFloat32 -> DXGI_FORMAT_R32_FLOAT
				ProgramAttributeFloat16 -> DXGI_FORMAT_R16_FLOAT
				ProgramAttributeInt32 ProgramAttributeNonNormalized -> DXGI_FORMAT_R32_SINT
				ProgramAttributeInt16 ProgramAttributeNonNormalized -> DXGI_FORMAT_R16_SINT
				ProgramAttributeInt16 ProgramAttributeNormalized -> DXGI_FORMAT_R16_SNORM
				ProgramAttributeInt8 ProgramAttributeNonNormalized -> DXGI_FORMAT_R8_SINT
				ProgramAttributeInt8 ProgramAttributeNormalized -> DXGI_FORMAT_R8_SNORM
				ProgramAttributeUint32 ProgramAttributeNonNormalized -> DXGI_FORMAT_R32_UINT
				ProgramAttributeUint16 ProgramAttributeNonNormalized -> DXGI_FORMAT_R16_UINT
				ProgramAttributeUint16 ProgramAttributeNormalized -> DXGI_FORMAT_R16_UNORM
				ProgramAttributeUint8 ProgramAttributeNonNormalized -> DXGI_FORMAT_R8_UINT
				ProgramAttributeUint8 ProgramAttributeNormalized -> DXGI_FORMAT_R8_UNORM
				ProgramAttributeVec1 a -> convertFormat a
				ProgramAttributeVec2 ProgramAttributeFloat32 -> DXGI_FORMAT_R32G32_FLOAT
				ProgramAttributeVec2 ProgramAttributeFloat16 -> DXGI_FORMAT_R16G16_FLOAT
				ProgramAttributeVec2 (ProgramAttributeInt32 ProgramAttributeNonNormalized) -> DXGI_FORMAT_R32G32_SINT
				ProgramAttributeVec2 (ProgramAttributeInt16 ProgramAttributeNonNormalized) -> DXGI_FORMAT_R16G16_SINT
				ProgramAttributeVec2 (ProgramAttributeInt16 ProgramAttributeNormalized) -> DXGI_FORMAT_R16G16_SNORM
				ProgramAttributeVec2 (ProgramAttributeInt8 ProgramAttributeNonNormalized) -> DXGI_FORMAT_R8G8_SINT
				ProgramAttributeVec2 (ProgramAttributeInt8 ProgramAttributeNormalized) -> DXGI_FORMAT_R8G8_SNORM
				ProgramAttributeVec2 (ProgramAttributeUint32 ProgramAttributeNonNormalized) -> DXGI_FORMAT_R32G32_UINT
				ProgramAttributeVec2 (ProgramAttributeUint16 ProgramAttributeNonNormalized) -> DXGI_FORMAT_R16G16_UINT
				ProgramAttributeVec2 (ProgramAttributeUint16 ProgramAttributeNormalized) -> DXGI_FORMAT_R16G16_UNORM
				ProgramAttributeVec2 (ProgramAttributeUint8 ProgramAttributeNonNormalized) -> DXGI_FORMAT_R8G8_UINT
				ProgramAttributeVec2 (ProgramAttributeUint8 ProgramAttributeNormalized) -> DXGI_FORMAT_R8G8_UNORM
				ProgramAttributeVec3 ProgramAttributeFloat32 -> DXGI_FORMAT_R32G32B32_FLOAT
				ProgramAttributeVec3 (ProgramAttributeInt32 ProgramAttributeNonNormalized) -> DXGI_FORMAT_R32G32B32_SINT
				ProgramAttributeVec3 (ProgramAttributeUint32 ProgramAttributeNonNormalized) -> DXGI_FORMAT_R32G32B32_UINT
				ProgramAttributeVec4 ProgramAttributeFloat32 -> DXGI_FORMAT_R32G32B32A32_FLOAT
				ProgramAttributeVec4 ProgramAttributeFloat16 -> DXGI_FORMAT_R16G16B16A16_FLOAT
				ProgramAttributeVec4 (ProgramAttributeInt32 ProgramAttributeNonNormalized) -> DXGI_FORMAT_R32G32B32A32_SINT
				ProgramAttributeVec4 (ProgramAttributeInt16 ProgramAttributeNonNormalized) -> DXGI_FORMAT_R16G16B16A16_SINT
				ProgramAttributeVec4 (ProgramAttributeInt16 ProgramAttributeNormalized) -> DXGI_FORMAT_R16G16B16A16_SNORM
				ProgramAttributeVec4 (ProgramAttributeInt8 ProgramAttributeNonNormalized) -> DXGI_FORMAT_R8G8B8A8_SINT
				ProgramAttributeVec4 (ProgramAttributeInt8 ProgramAttributeNormalized) -> DXGI_FORMAT_R8G8B8A8_SNORM
				ProgramAttributeVec4 (ProgramAttributeUint32 ProgramAttributeNonNormalized) -> DXGI_FORMAT_R32G32B32A32_UINT
				ProgramAttributeVec4 (ProgramAttributeUint16 ProgramAttributeNonNormalized) -> DXGI_FORMAT_R16G16B16A16_UINT
				ProgramAttributeVec4 (ProgramAttributeUint16 ProgramAttributeNormalized) -> DXGI_FORMAT_R16G16B16A16_UNORM
				ProgramAttributeVec4 (ProgramAttributeUint8 ProgramAttributeNonNormalized) -> DXGI_FORMAT_R8G8B8A8_UINT
				ProgramAttributeVec4 (ProgramAttributeUint8 ProgramAttributeNormalized) -> DXGI_FORMAT_R8G8B8A8_UNORM
				_ -> DXGI_FORMAT_UNKNOWN
			in inputElementDescs attributes []

		-- function to compile shader
		let compileShader HlslShader
			{ hlslShaderSource = source
			, hlslShaderEntryPoint = entryPoint
			, hlslShaderTarget = target
			} = do
			let debug = False
			let optimize = True
			let flags = fromIntegral $ (fromEnum D3DCOMPILE_ENABLE_STRICTNESS) .|. (fromEnum D3DCOMPILE_PACK_MATRIX_COLUMN_MAJOR)
				.|. (if debug then fromEnum D3DCOMPILE_DEBUG else 0)
				.|. (if optimize then fromEnum D3DCOMPILE_OPTIMIZATION_LEVEL3 else (fromEnum D3DCOMPILE_OPTIMIZATION_LEVEL0) .|. (fromEnum D3DCOMPILE_SKIP_OPTIMIZATION))
			(hr, shaderData, errorsData) <- BS.unsafeUseAsCStringLen (T.encodeUtf8 source) $ \(sourcePtr, sourceLen) -> do
				BS.unsafeUseAsCString (T.encodeUtf8 entryPoint) $ \entryPointPtr -> do
					BS.unsafeUseAsCString (T.encodeUtf8 target) $ \targetPtr -> do
						alloca $ \shaderBlobPtrPtr -> do
							alloca $ \errorsBlobPtrPtr -> do
								hr <- d3dCompile
									(castPtr sourcePtr) -- pSrcData
									(fromIntegral sourceLen) -- SrcDataSize
									nullPtr -- pSourceName
									nullPtr -- pDefines
									nullPtr -- pInclude
									entryPointPtr -- pEntrypoint
									targetPtr -- pTarget
									flags -- Flags1
									0 -- Flags2
									shaderBlobPtrPtr -- ppCode
									errorsBlobPtrPtr -- ppErrorMsgs
								-- retrieve blobs
								let getBlobData blobPtrPtr = do
									blobPtr <- peek blobPtrPtr
									if blobPtr == nullPtr then return BS.empty
									else do
										blobInterface <- peekCOMObject blobPtr
										dataPtr <- m_ID3DBlob_GetBufferPointer blobInterface
										dataSize <- liftM fromIntegral $ m_ID3DBlob_GetBufferSize blobInterface
										dataCopyPtr <- mallocBytes dataSize
										copyArray dataCopyPtr (castPtr dataPtr) dataSize
										_ <- m_IUnknown_Release blobInterface
										BS.unsafePackMallocCStringLen (dataCopyPtr, dataSize)
								shaderData <- getBlobData shaderBlobPtrPtr
								errorsData <- getBlobData errorsBlobPtrPtr
								return (hr, shaderData, errorsData)
			if hresultFailed hr then fail $ "failed to compile shader: " ++ BSC.unpack errorsData
			else return shaderData

		-- function to create vertex shader
		let createVertexShader bytecode = allocateCOMObject $ do
			BS.unsafeUseAsCStringLen bytecode $ \(ptr, len) -> do
				createCOMObjectViaPtr $ m_ID3D11Device_CreateVertexShader deviceInterface (castPtr ptr) (fromIntegral len) nullPtr
		-- function to create pixel shader
		let createPixelShader bytecode = allocateCOMObject $ do
			BS.unsafeUseAsCStringLen bytecode $ \(ptr, len) -> do
				createCOMObjectViaPtr $ m_ID3D11Device_CreatePixelShader deviceInterface (castPtr ptr) (fromIntegral len) nullPtr

		-- generate HLSL
		hlslProgram <- liftIO $ generateHlsl $ runReaderT program

		-- select on type of the program
		case hlslProgram of
			HlslVertexPixelProgram attributes vertexShader pixelShader -> do
				vertexShaderByteCode <- liftIO $ compileShader vertexShader
				(inputLayoutReleaseKey, inputLayoutInterface) <- createInputLayout attributes vertexShaderByteCode
				(vertexShaderReleaseKey, vertexShaderInterface) <- createVertexShader vertexShaderByteCode
				(pixelShaderReleaseKey, pixelShaderInterface) <- createPixelShader =<< (liftIO $ compileShader pixelShader)
				releaseKey <- register $ do
					_ <- release inputLayoutReleaseKey
					_ <- release vertexShaderReleaseKey
					_ <- release pixelShaderReleaseKey
					return ()
				return (releaseKey, Dx11VertexPixelProgramId inputLayoutInterface vertexShaderInterface pixelShaderInterface)

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
		(releaseKey, bufferInterface) <- allocateCOMObject $ with desc $ \descPtr -> do
			createCOMObjectViaPtr $ m_ID3D11Device_CreateBuffer deviceInterface descPtr nullPtr

		return (releaseKey, Dx11UniformBufferId bufferInterface)

-- | Create DirectX11 device.
createDx11Device :: (MonadResource m, MonadBaseControl IO m) => DeviceId DXGISystem -> m (ReleaseKey, Dx11Device, Dx11Context)
createDx11Device (DXGIDeviceId adapter) = describeException "failed to create DirectX11 graphics device" $ do
	-- create function
	let create = alloca $ \devicePtr -> alloca $ \deviceContextPtr -> do
		let featureLevels = [D3D_FEATURE_LEVEL_11_1, D3D_FEATURE_LEVEL_11_0, D3D_FEATURE_LEVEL_10_1, D3D_FEATURE_LEVEL_10_0]
		withArray featureLevels $ \featureLevelsPtr -> alloca $ \realFeatureLevelPtr -> do
			hresultCheck =<< d3d11CreateDevice (pokeCOMObject adapter) (wrapEnum D3D_DRIVER_TYPE_HARDWARE) nullPtr 0 featureLevelsPtr (fromIntegral $ length featureLevels) d3d11SdkVersion devicePtr realFeatureLevelPtr deviceContextPtr
		deviceInterface <- peekCOMObject =<< peek devicePtr
		contextInterface <- peekCOMObject =<< peek deviceContextPtr

		-- create context state
		contextState <- liftIO $ newIORef dx11DefaultContextState

		return (Dx11Device
			{ dx11DeviceInterface = deviceInterface
			, dx11DeviceImmediateContext = contextInterface
			}, Dx11Context
			{ dx11ContextInterface = contextInterface
			, dx11ContextState = contextState
			})

	-- destroy function
	let destroy (Dx11Device
		{ dx11DeviceInterface = deviceInterface
		}, Dx11Context
		{ dx11ContextInterface = contextInterface
		}) = do
			_ <- m_IUnknown_Release deviceInterface
			_ <- m_IUnknown_Release contextInterface
			return ()

	-- perform creation
	(releaseKey, (device, context)) <- allocate create destroy
	return (releaseKey, device, context)

-- | DirectX11 graphics context.
data Dx11Context = Dx11Context
	{ dx11ContextInterface :: ID3D11DeviceContext
	, dx11ContextState :: IORef Dx11ContextState
	}

type Dx11ContextState = RenderState Dx11Device

dx11DefaultRenderState :: RenderState Dx11Device
dx11DefaultRenderState = RenderState
	{ renderStateFrameBuffer = Dx11FrameBufferId [] Dx11NullDepthStencilTargetId
	, renderStateViewport = (0, 0)
	, renderStateVertexBuffers = []
	, renderStateIndexBuffer = Dx11NullIndexBufferId
	, renderStateSamplers = []
	, renderStateProgram = Dx11NullProgramId
	}

dx11DefaultContextState :: Dx11ContextState
dx11DefaultContextState = dx11DefaultRenderState

instance Context Dx11Context Dx11Device where
	contextReset Dx11Context
		{ dx11ContextInterface = contextInterface
		, dx11ContextState = contextState
		} = do
		m_ID3D11DeviceContext_ClearState contextInterface
		writeIORef contextState dx11DefaultContextState
		return dx11DefaultRenderState

	contextClearColor Dx11Context
		{ dx11ContextInterface = contextInterface
		} RenderState
		{ renderStateFrameBuffer = Dx11FrameBufferId renderTargets _depthStencilTarget
		} targetIndex color = do
		let Dx11RenderTargetId rtvInterface = renderTargets !! targetIndex
		withArray (vecToList color) $ \colorPtr -> do
			m_ID3D11DeviceContext_ClearRenderTargetView contextInterface (pokeCOMObject rtvInterface) colorPtr

	contextClearDepth context renderState depth = do
		dx11ClearDepthStencil context renderState depth 0 (fromEnum D3D11_CLEAR_DEPTH)

	contextClearStencil context renderState stencil = do
		dx11ClearDepthStencil context renderState 0 stencil (fromEnum D3D11_CLEAR_STENCIL)

	contextClearDepthStencil context renderState depth stencil = do
		dx11ClearDepthStencil context renderState depth stencil ((fromEnum D3D11_CLEAR_DEPTH) .|. (fromEnum D3D11_CLEAR_STENCIL))

	contextDraw context@Dx11Context
		{ dx11ContextInterface = contextInterface
		} renderState@RenderState
		{ renderStateIndexBuffer = indexBuffer
		} indicesCount = do
		dx11UpdateContext context renderState
		case indexBuffer of
			Dx11IndexBufferId _indexBufferInterface -> do
				m_ID3D11DeviceContext_DrawIndexed contextInterface (fromIntegral indicesCount) 0 0
			Dx11NullIndexBufferId -> do
				m_ID3D11DeviceContext_Draw contextInterface (fromIntegral indicesCount) 0

-- | Helper method to clear depth and/or stencil.
dx11ClearDepthStencil :: Dx11Context -> RenderState Dx11Device -> Float -> Int -> Int -> IO ()
dx11ClearDepthStencil Dx11Context
	{ dx11ContextInterface = contextInterface
	} RenderState
	{ renderStateFrameBuffer = Dx11FrameBufferId _renderTargets depthStencilTarget
	} depth stencil flags = do
	case depthStencilTarget of
		Dx11DepthStencilTargetId dsvInterface -> do
			m_ID3D11DeviceContext_ClearDepthStencilView contextInterface (pokeCOMObject dsvInterface) (fromIntegral flags) depth (fromIntegral stencil)
		Dx11NullDepthStencilTargetId -> return ()

-- | Update context.
dx11UpdateContext :: Dx11Context -> RenderState Dx11Device -> IO ()
dx11UpdateContext Dx11Context
	{ dx11ContextInterface = contextInterface
	, dx11ContextState = actualContextState
	} RenderState
	{ renderStateFrameBuffer = desiredFrameBuffer
	, renderStateViewport = desiredViewport
	, renderStateVertexBuffers = desiredVertexBuffers
	, renderStateIndexBuffer = desiredIndexBuffers
	, renderStateUniformBuffers = desiredUniformBuffers
	, renderStateSamplers = desiredSamplers
	, renderStateProgram = desiredProgram
	} = do
	-- unpack actual render state
	RenderState
		{ renderStateFrameBuffer = actualFrameBuffer
		, renderStateViewport = actualViewport
		, renderStateVertexBuffers = actualVertexBuffers
		, renderStateIndexBuffer = actualIndexBuffer
		, renderStateUniformBuffers = actualUniformBuffers
		, renderStateSamplers = actualSamplers
		, renderStateProgram = actualProgram
		} <- readIORef actualContextState

	-- framebuffer
	if actualFrameBuffer /= desiredFrameBuffer then do
		let Dx11FrameBufferId renderTargets depthStencilTarget = desiredFrameBuffer
		let renderTargetsInterfaces = [pokeCOMObject renderTargetInterface | Dx11RenderTargetId renderTargetInterface <- renderTargets]
		let depthStencilInterface = case depthStencilTarget of
			Dx11DepthStencilTargetId interface -> pokeCOMObject interface
			Dx11NullDepthStencilTargetId -> nullPtr
		withArray renderTargetsInterfaces $ \renderTargetsInterfacesPtr -> do
			m_ID3D11DeviceContext_OMSetRenderTargets contextInterface (fromIntegral $ length renderTargetsInterfaces) renderTargetsInterfacesPtr depthStencilInterface
	else return ()

	-- samplers
	if actualSamplers /= desiredSamplers then do
		let samplersCount = fromIntegral $ length desiredSamplers
		let srvInterfaces = [case texture of
			Dx11TextureId srvInterface -> pokeCOMObject srvInterface
			Dx11NullTextureId -> nullPtr
			| (texture, _sampler) <- desiredSamplers]
		withArray srvInterfaces $ \srvInterfacesPtr -> do
			m_ID3D11DeviceContext_VSSetShaderResources contextInterface 0 samplersCount srvInterfacesPtr
			m_ID3D11DeviceContext_PSSetShaderResources contextInterface 0 samplersCount srvInterfacesPtr
		let ssInterfaces = [pokeCOMObject ssInterface | (_texture, Dx11SamplerStateId ssInterface) <- desiredSamplers]
		withArray ssInterfaces $ \ssInterfacesPtr -> do
			m_ID3D11DeviceContext_VSSetSamplers contextInterface 0 samplersCount ssInterfacesPtr
			m_ID3D11DeviceContext_PSSetSamplers contextInterface 0 samplersCount ssInterfacesPtr
	else return ()

	-- uniform buffers
	if actualUniformBuffers /= desiredUniformBuffers then do
		let buffersCount = fromIntegral $ length desiredUniformBuffers
		let buffersInterfaces = [case uniformBuffer of
			Dx11UniformBufferId bufferInterface -> pokeCOMObject bufferInterface
			Dx11NullUniformBufferId -> nullPtr
			| uniformBuffer <- desiredUniformBuffers]
		withArray buffersInterfaces $ \buffersInterfacesPtr -> do
			m_ID3D11DeviceContext_VSSetConstantBuffers contextInterface 0 buffersCount buffersInterfacesPtr
			m_ID3D11DeviceContext_PSSetConstantBuffers contextInterface 0 buffersCount buffersInterfacesPtr
	else return ()

	-- program (shaders, input layout)
	if actualProgram /= desiredProgram then do
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
	else return ()

	-- vertex buffers
	if actualVertexBuffers /= desiredVertexBuffers then do
		let buffersCount = fromIntegral $ length desiredVertexBuffers
		let (buffersInterfaces, strides) = unzip [case vertexBuffer of
			Dx11VertexBufferId bufferInterface stride -> (pokeCOMObject bufferInterface, stride)
			Dx11NullVertexBufferId -> (nullPtr, 0)
			| vertexBuffer <- desiredVertexBuffers]
		withArray buffersInterfaces $ \buffersInterfacesPtr -> do
			withArray (map fromIntegral strides) $ \stridesPtr -> do
				withArray (replicate buffersCount 0) $ \offsetsPtr -> do
					m_ID3D11DeviceContext_IASetVertexBuffers contextInterface 0 (fromIntegral buffersCount) buffersInterfacesPtr stridesPtr offsetsPtr
	else return ()
