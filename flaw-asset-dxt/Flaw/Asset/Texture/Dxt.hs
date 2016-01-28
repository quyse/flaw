{-|
Module: Flaw.Asset.Texture.Dxt
Description: DXT texture compression and decompression.
License: MIT
-}

module Flaw.Asset.Texture.Dxt
	( dxtCompressTexture
	) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr

import Flaw.Exception
import Flaw.Graphics.Texture

dxtCompressTexture :: TextureInfo -> B.ByteString -> IO (TextureInfo, B.ByteString)
dxtCompressTexture textureInfo@TextureInfo
	{ textureHeight = height
	, textureFormat = format
	, textureCount = count
	} bytes = do

	-- texture must be at least 2D
	when (height == 0) $ throwIO $ DescribeFirstException "DXT compression doesn't support 1D textures"

	-- get compression function
	(compressBlock, newFormat) <- case format of
		UncompressedTextureFormat
			{ textureFormatComponents = PixelRGB
			, textureFormatValueType = PixelUint
			, textureFormatPixelSize = Pixel24bit
			, textureFormatColorSpace = colorSpace
			} -> return (compressBC1Block, CompressedTextureFormat
			{ textureFormatCompression = TextureCompressionBC1
			, textureFormatColorSpace = colorSpace
			})
		UncompressedTextureFormat
			{ textureFormatComponents = PixelR
			, textureFormatValueType = PixelUint
			, textureFormatPixelSize = Pixel8bit
			, textureFormatColorSpace = colorSpace
			} -> return (compressBC4Block, CompressedTextureFormat
			{ textureFormatCompression = TextureCompressionBC4
			, textureFormatColorSpace = colorSpace
			})
		_ -> throwIO $ DescribeFirstException $ "unsupported format for DXT compression: " ++ show format

	-- new texture info
	let newTextureInfo = textureInfo
		{ textureFormat = newFormat
		}

	-- calculate metrics
	let TextureMetrics
		{ textureImageSize = imageSize
		, textureMipsMetrics = mipsMetrics
		} = calcTextureMetrics textureInfo
	let TextureMetrics
		{ textureImageSize = newImageSize
		, textureMipsMetrics = newMipsMetrics
		} = calcTextureMetrics newTextureInfo

	-- number of images
	let ncount = if count > 0 then count else 1

	-- total size of new image array
	let newTotalImageSize = newImageSize * ncount

	-- size of input pixel
	let pixelByteSize = pixelSizeByteSize $ textureFormatPixelSize format
	-- size of output block
	let blockByteSize = compressed4x4BlockSize $ textureFormatCompression newFormat

	-- allocate memory
	newBytesPtr <- mallocBytes newTotalImageSize

	-- process source image
	B.unsafeUseAsCString bytes $ \bytesPtr -> do
		-- loop for textures in the array
		forM_ [0..(ncount - 1)] $ \c -> do
			let imageSrcPtr = bytesPtr `plusPtr` (c * imageSize)
			let imageDestPtr = newBytesPtr `plusPtr` (c * newImageSize)
			-- loop for mips
			forM_ (zip mipsMetrics newMipsMetrics) $ \(TextureMipMetrics
				{ textureMipWidth = mipWidth
				, textureMipHeight = mipHeight
				, textureMipDepth = mipDepth
				, textureMipLinePitch = mipLinePitch
				, textureMipSlicePitch = mipSlicePitch
				, textureMipOffset = mipOffset
				}, TextureMipMetrics
				{ textureMipLinePitch = newMipLinePitch
				, textureMipSlicePitch = newMipSlicePitch
				, textureMipOffset = newMipOffset
				}) -> do
				let mipSrcPtr = imageSrcPtr `plusPtr` mipOffset
				let mipDestPtr = imageDestPtr `plusPtr` newMipOffset
				let cMipLinePitch = fromIntegral mipLinePitch
				-- loop for depth slices
				forM_ [0..(mipDepth - 1)] $ \k -> do
					let sliceSrcPtr = mipSrcPtr `plusPtr` (k * mipSlicePitch)
					let sliceDestPtr = mipDestPtr `plusPtr` (k * newMipSlicePitch)
					-- loop for lines of 4x4 blocks
					forM_ [0,4..(mipHeight - 1)] $ \i -> do
						let lineSrcPtr = sliceSrcPtr `plusPtr` (i * mipLinePitch)
						let lineDestPtr = sliceDestPtr `plusPtr` ((i `div` 4) * newMipLinePitch)
						-- loop for 4x4 blocks
						forM_ [0,4..(mipWidth - 1)] $ \j -> do
							let blockSrcPtr = lineSrcPtr `plusPtr` (j * pixelByteSize)
							let blockDestPtr = lineDestPtr `plusPtr` ((j `div` 4) * blockByteSize)
							-- compress block
							-- TODO: handle source final (partial) blocks
							compressBlock blockSrcPtr cMipLinePitch blockDestPtr

	-- pack memory into bytestring
	newBytes <- B.unsafePackMallocCStringLen (newBytesPtr, newTotalImageSize)
	return (newTextureInfo, newBytes)

foreign import ccall unsafe "flaw_squish_compress_bc1" compressBC1Block :: Ptr () -> CInt -> Ptr () -> IO ()
foreign import ccall unsafe "flaw_squish_compress_bc4" compressBC4Block :: Ptr () -> CInt -> Ptr () -> IO ()
