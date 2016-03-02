{-|
Module: Flaw.Visual.Geometry.Skinned
Description: Skinned geometry support.
License: MIT
-}

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Flaw.Visual.Geometry.Skinned
	( SkinnedGeometry(..)
	, SkinnedGeometryAnimation(..)
	, textureAnimateSkinnedGeometry
	, emitTextureAnimatedSkinnedGeometryAsset
	, loadTextureAnimatedSkinnedGeometryAsset
	, textureAnimatedSkinTransform
	) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics(Generic)
import Language.Haskell.TH

import Flaw.Asset.Collada
import Flaw.Book
import Flaw.Build
import Flaw.Exception
import Flaw.Graphics
import Flaw.Graphics.Program
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
	, skinnedGeometryAnimations :: !(V.Vector SkinnedGeometryAnimation)
	}

data PackedSkinnedGeometry = PackedSkinnedGeometry
	{ packedSkinnedGeometryGeometry :: !PackedGeometry
	, packedSkinnedGeometryAnimationTexture :: (TextureInfo, B.ByteString)
	, packedSkinnedGeometryAnimationOffsetsBytes :: !B.ByteString
	, packedSkinnedGeometryAnimationInvLength :: {-# UNPACK #-} !Float
	} deriving Generic

instance S.Serialize PackedSkinnedGeometry

data SkinnedGeometryAnimation = SkinnedGeometryAnimation
	{ skinnedGeometryAnimationOffset :: {-# UNPACK #-} !Float4
	, skinnedGeometryAnimationInvLength :: {-# UNPACK #-} !Float
	} deriving Generic

{-# INLINE textureAnimateSkinnedGeometry #-}
textureAnimateSkinnedGeometry :: SkinnedGeometryAnimation -> Float -> Float4
textureAnimateSkinnedGeometry SkinnedGeometryAnimation
	{ skinnedGeometryAnimationOffset = Float4 kx ky x y
	, skinnedGeometryAnimationInvLength = invLength
	} t = Float4 kx ky (x + t * invLength) y

-- | Return asset representing skinned geometry with animations taken from Collada file.
emitTextureAnimatedSkinnedGeometryAsset :: FilePath -> ColladaM ColladaElement -> ColladaM ColladaElement -> Float -> Float -> Q B.ByteString
emitTextureAnimatedSkinnedGeometryAsset fileName getNodeElement getSkinElement timeStep timeLength = do
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
			let msg = "failed to embed skinned geometry " ++ fileName ++ ": " ++ T.unpack err
			reportError msg
			return B.empty
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
			return $ S.encode $ PackedSkinnedGeometry
				{ packedSkinnedGeometryGeometry = packedGeometry
				, packedSkinnedGeometryAnimationTexture = (textureInfo, textureBytes)
				, packedSkinnedGeometryAnimationOffsetsBytes = animationOffsetsBytes
				, packedSkinnedGeometryAnimationInvLength = 1 / timeLength
				}

loadTextureAnimatedSkinnedGeometryAsset :: Device d => d -> B.ByteString -> IO (SkinnedGeometry d, IO ())
loadTextureAnimatedSkinnedGeometryAsset device bytes = case S.decode bytes of
	Left err -> throwIO $ DescribeFirstException $ "failed to load texture animated skinned geometry asset: " ++ err
	Right PackedSkinnedGeometry
		{ packedSkinnedGeometryGeometry = packedGeometry
		, packedSkinnedGeometryAnimationTexture = (textureInfo, textureBytes)
		, packedSkinnedGeometryAnimationOffsetsBytes = animationOffsetsBytes
		, packedSkinnedGeometryAnimationInvLength = animationInvLength
		} -> withSpecialBook $ \bk -> do
		geometry <- book bk $ loadPackedGeometry device packedGeometry
		animationTexture <- book bk $ createStaticTexture device textureInfo defaultSamplerStateInfo
			{ samplerMinFilter = SamplerLinearFilter
			, samplerMipFilter = SamplerPointFilter
			, samplerMagFilter = SamplerLinearFilter
			, samplerWrapU = SamplerWrapClamp
			, samplerWrapV = SamplerWrapClamp
			, samplerWrapW = SamplerWrapClamp
			} textureBytes
		animationOffsets <- unpackVector animationOffsetsBytes
		let animations = flip V.map animationOffsets $ \animationOffset -> SkinnedGeometryAnimation
			{ skinnedGeometryAnimationOffset = animationOffset
			, skinnedGeometryAnimationInvLength = animationInvLength
			}
		return SkinnedGeometry
			{ skinnedGeometryGeometry = geometry
			, skinnedGeometryAnimationTexture = animationTexture
			, skinnedGeometryAnimations = animations
			}

-- | Skin transform based on animation written to texture.
textureAnimatedSkinTransform :: Int -> Node Float4 -> Node Float4 -> Node Word32_4 -> Program (Node Float4, Node Float3)
textureAnimatedSkinTransform animationSamplerSlot animationOffset weights bones = do
	-- function getting quaternion and offset for a bone
	let transformBone boneOffsetY = do
		let s = sampler2D4f animationSamplerSlot
		c <- temp $ cvec11 (z_ animationOffset) boneOffsetY
		return
			( sampleLod s c (constf 0)
			, xyz__ $ sampleLod s (c + cvec11 0 (y_ animationOffset)) (constf 0)
			)

	-- get bone offsets for bones of current vertex
	boneOffsetYs <- temp $ xxxx__ animationOffset * cast bones + wwww__ animationOffset

	-- get quatoffsets for vertex' bones
	(q1, p1) <- transformBone $ x_ boneOffsetYs
	(q2, p2) <- transformBone $ y_ boneOffsetYs
	(q3, p3) <- transformBone $ z_ boneOffsetYs
	(q4, p4) <- transformBone $ w_ boneOffsetYs

	-- mix transforms
	q <- temp $ normalize
		$ q1 * xxxx__ weights
		+ q2 * yyyy__ weights
		+ q3 * zzzz__ weights
		+ q4 * wwww__ weights
	p <- temp
		$ p1 * xxx__ weights
		+ p2 * yyy__ weights
		+ p3 * zzz__ weights
		+ p4 * www__ weights

	return (q, p)
