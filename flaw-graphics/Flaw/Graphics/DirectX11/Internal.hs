{-|
Module: Flaw.Graphics.DirectX11.Internal
Description: Internals of graphics implementation for DirectX 11.
License: MIT
-}

{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Flaw.Graphics.DirectX11.Internal
	( Dx11Device(..)
	, TextureId(..)
	, RenderTargetId(..)
	, VertexLayoutId(..)
	, VertexBufferId(..)
	, createDx11Device
	) where

import qualified Control.Exception.Lifted as Lifted
import Control.Monad
import Control.Monad.Trans.Resource
import Data.Bits
import qualified Data.ByteString.Unsafe as BS
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Flaw.Exception
import Flaw.FFI
import Flaw.FFI.COM
import Flaw.Graphics.Abstract
import Flaw.Graphics.DirectX11.FFI
import Flaw.Graphics.DXGI.FFI
import Flaw.Graphics.DXGI.Internal
import Flaw.Graphics.Internal
import Flaw.Graphics.Texture

-- | DirectX11 graphics device.
data Dx11Device = Dx11Device
	{ dx11DeviceInterface :: ID3D11Device
	, dx11DeviceImmediateContext :: ID3D11DeviceContext
	}

instance Device Dx11Device where
	type DeferredContext Dx11Device = Dx11DeferredContext
	newtype TextureId Dx11Device = Dx11TextureId ID3D11ShaderResourceView
	newtype RenderTargetId Dx11Device = Dx11RenderTargetId ID3D11RenderTargetView
	newtype DepthStencilTargetId Dx11Device = Dx11DepthStencilTargetId ID3D11DepthStencilView
	data FrameBufferId Dx11Device = Dx11FrameBufferId [RenderTargetId Dx11Device] (Maybe (DepthStencilTargetId Dx11Device))
	newtype VertexLayoutId Dx11Device = Dx11VertexLayoutId VertexLayoutInfo
	newtype VertexBufferId Dx11Device = Dx11VertexBufferId ID3D11Buffer
	newtype IndexBufferId Dx11Device = Dx11IndexBufferId ID3D11Buffer
	newtype VertexShaderId Dx11Device = Dx11VertexShaderId ID3D11VertexShader
	newtype PixelShaderId Dx11Device = Dx11PixelShaderId ID3D11PixelShader
	data ProgramId Dx11Device = Dx11ProgramId ID3D11VertexShader ID3D11PixelShader

	createDeferredContext Dx11Device
		{ dx11DeviceInterface = deviceInterface
		} = describeException "failed to create DirectX11 deferred context" $ do
		(releaseKey, deviceContext) <- allocateCOMObject $ createCOMObjectViaPtr $ m_ID3D11Device_CreateDeferredContext deviceInterface 0
		return (releaseKey, Dx11DeferredContext deviceContext)

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

-- | Create DirectX11 device.
createDx11Device :: (MonadResource m, MonadBaseControl IO m) => DeviceId DXGISystem -> m (ReleaseKey, Dx11Device)
createDx11Device (DXGIDeviceId adapter) = describeException "failed to create DirectX11 graphics device" $ do
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
		, dx11DeviceImmediateContext = deviceContext
		})

newtype Dx11DeferredContext = Dx11DeferredContext ID3D11DeviceContext
