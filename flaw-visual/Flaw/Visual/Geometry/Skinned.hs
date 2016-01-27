{-|
Module: Flaw.Visual.Geometry.Skinned
Description: Skinned geometry support.
License: MIT
-}

{-# LANGUAGE TemplateHaskell #-}

module Flaw.Visual.Geometry.Skinned
	( SkinnedGeometry(..)
	, embedLoadSkinnedGeometryExp
	) where

import Control.Monad
import qualified Data.ByteString.Unsafe as B
import qualified Data.Vector as V
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Language.Haskell.TH
import qualified Text.XML.Light as XML

import Flaw.Asset.Collada
import Flaw.Book
import Flaw.Build
import Flaw.Graphics
import Flaw.Graphics.Sampler
import Flaw.Graphics.Texture
import Flaw.Math
import Flaw.Math.Transform
import Flaw.Visual.Geometry
import Flaw.Visual.Geometry.Vertex
import Flaw.Visual.Texture()

data SkinnedGeometry d = SkinnedGeometry
	{ skinnedGeometryGeometry :: !(Geometry d)
	, skinnedGeometryAnimationTexture :: !(TextureId d)
	, skinnedGeometryAnimations :: [Float -> Float4]
	}

-- | Generate expression for loading embedded skinned geometry with animations taken from Collada file.
-- Expression type is :: Device d => IO (SkinnedGeometry d, IO ())
embedLoadSkinnedGeometryExp :: FilePath -> ColladaM XML.Element -> ColladaM XML.Element -> Float -> Float -> ExpQ
embedLoadSkinnedGeometryExp fileName getNodeElement getSkinElement timeStep timeLength = do
	-- load file
	bytes <- loadFile fileName

	-- parse collada
	let eitherResult = runCollada $ do
		initColladaCache bytes
		skeleton <- parseSkeleton =<< getNodeElement
		animations <- mapM (animateSkeleton skeleton <=< parseAnimation) =<< getAllElementsByTag "animation"
		(skinVerticesData, skin) <- parseSkin skeleton =<< getSingleChildWithTag "skin" =<< getSkinElement
		vertices <- createColladaVertices skinVerticesData
		return (vertices, skin, V.fromList animations)

	case eitherResult of
		Left err -> do
			let msg = "failed to embed skinned geometry " ++ fileName ++ ": " ++ err
			reportError msg
			[| error msg |]
		Right (vertices, skin, animations) -> do
			packedGeometry <- runIO $ packGeometry (vertices :: V.Vector VertexPNTWB)
			let framesCount = floor $ timeLength / timeStep
			let width = framesCount
			let bones = cskinBones skin
			let bonesCount = V.length bones
			let height = V.length animations * bonesCount * 2
			ptr <- runIO $ mallocArray $ width * height
			runIO $ forM_ [0 .. (V.length animations - 1)] $ \animationIndex -> do
				let animate = animations V.! animationIndex
				forM_ [0 .. (framesCount - 1)] $ \frame -> do
					let transforms = animate identityTransform $ fromIntegral frame * timeStep
					flip V.imapM_ bones $ \i ColladaBone
						{ cboneSkeletonIndex = boneIndex
						, cboneInvBindTransform = boneInvBindTransform
						} -> do
						let QuatOffset (FloatQ q) (Float3 px py pz) = transformFromMatrix $ combineTransform (transforms V.! boneIndex) boneInvBindTransform
						let offset = (animationIndex * bonesCount + i) * width * 2 + frame
						pokeElemOff ptr offset $ normalize q
						pokeElemOff ptr (offset + width) $ Vec4 px py pz 0
			textureBytes <- runIO $ B.unsafePackMallocCStringLen (castPtr ptr, width * height * sizeOf (undefined :: Float4))
			let textureInfo = TextureInfo
				{ textureWidth = width
				, textureHeight = height
				, textureDepth = 0
				, textureMips = 1
				, textureFormat = UncompressedTextureFormat
					{ textureFormatComponents = PixelRGBA
					, textureFormatValueType = PixelFloat
					, textureFormatPixelSize = Pixel128bit
					, textureFormatColorSpace = LinearColorSpace
					}
				, textureCount = 0
				}
			let animationOffsets = V.generate (V.length animations) $ \i -> Float4
				(2 / fromIntegral height) (1 / fromIntegral height)
				(0.5 / fromIntegral width) (fromIntegral i / fromIntegral (V.length animations) + 0.5 / fromIntegral height)
			animationOffsetsBytes <- runIO $ packVector animationOffsets
			[| \device -> withSpecialBook $ \bk -> do
				geometry <- book bk $ loadPackedGeometry device $(embedExp packedGeometry)
				at <- book bk $ createStaticTexture device $(embedExp textureInfo) defaultSamplerStateInfo
					{ samplerMinFilter = SamplerLinearFilter
					, samplerMipFilter = SamplerPointFilter
					, samplerMagFilter = SamplerLinearFilter
					, samplerWrapU = SamplerWrapClamp
					, samplerWrapV = SamplerWrapClamp
					, samplerWrapW = SamplerWrapClamp
					} $(embedExp textureBytes)
				aos <- B.unsafeUseAsCString $(embedExp animationOffsetsBytes) $ peekArray $(litE $ integerL $ fromIntegral $ V.length animations) . castPtr
				let ao = flip map aos $ \(Float4 kx ky x y) -> \t -> Float4 kx ky (x + t * $(litE $ rationalL $ 1 / toRational timeLength)) y
				return SkinnedGeometry
					{ skinnedGeometryGeometry = geometry
					, skinnedGeometryAnimationTexture = at
					, skinnedGeometryAnimations = ao
					}
				|]
