{-|
Module: Flaw.Graphics.Program
Description: Shader program support.
License: MIT
-}

{-# LANGUAGE TemplateHaskell #-}

module Flaw.Graphics.Program
	( Program
	, OfScalarType(..)
	, OfAttributeType(..)
	, AttributeFormat(..)
	, AttributeType(..)
	, Normalization(..)
	, Node
	, cnst
	, constf, const2f, const3f, const4f
	, cvec11, cvec111, cvec12, cvec21, cvec1111, cvec112, cvec121, cvec211, cvec22, cvec13, cvec31
	, cast
	, attribute
	, attributeWithType
	, UniformBufferSlot
	, UniformStorage
	, uniformBufferSlot
	, uniform
	, uniformArray
	, createUniformStorage
	, setUniform
	, renderUniform
	, renderIndexedUniform
	, renderUploadUniformStorage
	, renderUniformStorage
	, sampler
	, sampler1Df, sampler1D2f, sampler1D3f, sampler1D4f
	, sampler2Df, sampler2D2f, sampler2D3f, sampler2D4f
	, sampler3Df, sampler3D2f, sampler3D3f, sampler3D4f
	, samplerCubef, samplerCube2f, samplerCube3f, samplerCube4f
	, sample, sampleOffset, sampleLod, sampleLodOffset, sampleBias, sampleBiasOffset, sampleGrad, sampleGradOffset
	, temp
	, rasterize
	, colorTarget
	, dualColorTarget
	, depthTarget
	, (!)
	, min_, max_
	, clamp, lerp
	, equal_ , less_, lessEqual_, if_
	, ddx, ddy
	, instanceId
	, invSqrt
	, screenToTexture
	, normalizeSampledDepth
	, fragCoord
	) where

import Control.Monad.Reader
import qualified Data.ByteString.Unsafe as B
import Data.Char
import Data.Int
import Data.IORef
import Data.Word
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import Foreign.Storable
import Language.Haskell.TH

import Flaw.Graphics
import Flaw.Graphics.Program.Internal
import Flaw.Math

cnst :: OfValueType a => a -> Node a
cnst value = ConstNode (valueType value) value

-- | Helper method to know return value.
withUndefined :: (a -> Node a) -> Node a
withUndefined q = q undefined

-- | Helper method to know return value in monad.
withUndefinedM :: (a -> m (Node a)) -> m (Node a)
withUndefinedM q = q undefined

-- | Create vector as a combination of scalars/vectors.
fmap concat $ forM
	[ [1, 1]
	, [1, 1, 1], [1, 2], [2, 1]
	, [1, 1, 1, 1], [1, 1, 2], [1, 2, 1], [2, 1, 1], [2, 2], [1, 3], [3, 1]]
	$ \cs -> do
	ps <- forM (zip cs [1..]) $ \(_c, i) -> newName ['p', intToDigit i]
	let funName = mkName $ "cvec" ++ map intToDigit cs
	tvA <- newName "a"
	u <- newName "u"
	let vecType n = appT (conT $ mkName $ "Vec" ++ [intToDigit n]) (varT tvA)
	let argType n = [t| Node $(if n > 1 then vecType n else varT tvA) |]
	let funType = forallT [PlainTV tvA] (sequence [ [t| OfScalarType $(varT tvA) |], [t| Vectorized $(varT tvA) |] ]) $
		foldr (\a b -> [t| $a -> $b |]) [t| Node $(vecType $ sum cs) |] $ map argType cs
	let construction = foldl appE (conE $ mkName $ "Combine" ++ [intToDigit $ length cs] ++ "VecNode") $
		(map (\a -> [| nodeValueType $(varE a) |]) ps) ++ [ [| valueType $(varE u) |] ] ++ (map varE ps)
	sequence
		[ sigD funName funType
		, funD funName [clause (map varP ps) (normalB [| withUndefined $ \ $(varP u) -> $construction |]) []]
		]

-- | Cast value to other type.
cast :: (OfValueType a, OfValueType b) => Node a -> Node b
cast a = withUndefined $ \u -> CastNode (nodeValueType a) (valueType u) a

attribute :: OfAttributeType a => Int -> Int -> Int -> AttributeFormat a -> Program (Node a)
attribute slot offset divisor format = attributeWithType slot offset divisor $ attributeFormatToType format

attributeWithType :: OfAttributeType a => Int -> Int -> Int -> AttributeType -> Program (Node a)
attributeWithType slot offset divisor at = withUndefinedM $ \u -> withState $ \state@State
	{ stateStage = stage
	} -> do
	if stage /= VertexStage then fail "attribute can only be defined in vertex program"
	else return ()
	tempInternal (AttributeNode Attribute
		{ attributeSlot = slot
		, attributeOffset = offset
		, attributeDivisor = divisor
		, attributeType = at
		, attributeValueType = valueType u
		}) state

data UniformBufferSlot = UniformBufferSlot
	{ uniformBufferSlotIndex :: Int
	, uniformBufferSlotSizeRef :: IORef Int
	}

-- | Helper object for uniform buffer.
data UniformStorage d = UniformStorage
	{ uniformStorageSlot :: Int
	, uniformStorageBufferId :: UniformBufferId d
	, uniformStorageBytes :: ForeignPtr ()
	, uniformStorageSize :: Int
	}

uniformBufferSlot :: Int -> IO UniformBufferSlot
uniformBufferSlot slot = do
	sizeRef <- newIORef 0
	return UniformBufferSlot
		{ uniformBufferSlotIndex = slot
		, uniformBufferSlotSizeRef = sizeRef
		}

-- | Modified alignment calculation for shaders.
-- Standard math types return alignment only for their inner components, so:
-- > alignment (undefined :: Float3) = alignment (undefined :: Float)
-- This function calculates more realistic alignment for shader programs.
shaderAlignment :: Storable a => a -> Int
shaderAlignment u = f (sizeOf u) (sizeOf (undefined :: Float)) where
	f n s
		| n <= s || s >= (sizeOf (undefined :: Float4)) = s
		| otherwise = f n (s * 2)

uniform :: (OfValueType a, Storable a) => UniformBufferSlot -> IO (Node a)
uniform UniformBufferSlot
	{ uniformBufferSlotIndex = slot
	, uniformBufferSlotSizeRef = sizeRef
	} = withUndefinedM $ \u -> do
	bufferSize <- readIORef sizeRef
	let align = shaderAlignment u
	let alignedBufferSize = ((bufferSize + align - 1) `quot` align) * align
	writeIORef sizeRef $ alignedBufferSize + sizeOf u
	return $ UniformNode Uniform
		{ uniformSlot = slot
		, uniformOffset = alignedBufferSize
		, uniformSize = 0
		, uniformType = valueType u
		}

uniformArray :: (OfValueType a, Storable a) => Int -> UniformBufferSlot -> IO (Node [a])
uniformArray size UniformBufferSlot
	{ uniformBufferSlotIndex = slot
	, uniformBufferSlotSizeRef = sizeRef
	} = wu func where
	wu :: (a -> IO (Node [a])) -> IO (Node [a])
	wu f = f undefined
	func u = do
		bufferSize <- readIORef sizeRef
		let align = shaderAlignment u
		let alignedBufferSize = ((bufferSize + align - 1) `quot` align) * align
		writeIORef sizeRef $ alignedBufferSize + (sizeOf u) * size
		return $ UniformNode Uniform
			{ uniformSlot = slot
			, uniformOffset = alignedBufferSize
			, uniformSize = size
			, uniformType = valueType u
			}

createUniformStorage :: Device d => d -> UniformBufferSlot -> IO (UniformStorage d, IO ())
createUniformStorage device UniformBufferSlot
	{ uniformBufferSlotIndex = slot
	, uniformBufferSlotSizeRef = sizeRef
	} = do
	size <- readIORef sizeRef
	-- align just in case
	let alignedSize = ((size + 15) `quot` 16) * 16
	(uniformBuffer, release) <- createUniformBuffer device alignedSize
	bytes <- mallocForeignPtrBytes alignedSize
	return (UniformStorage
		{ uniformStorageSlot = slot
		, uniformStorageBufferId = uniformBuffer
		, uniformStorageBytes = bytes
		, uniformStorageSize = alignedSize
		}, release)

setUniform :: (OfValueType a, Storable a) => UniformStorage d -> Node a -> a -> IO ()
setUniform UniformStorage
	{ uniformStorageBytes = bytes
	} (UniformNode Uniform
	{ uniformOffset = offset
	}) value = do
	withForeignPtr bytes $ \ptr -> do
		pokeByteOff ptr offset value
setUniform _ _ _ = undefined

renderUniform :: (OfValueType a, Storable a) => UniformStorage d -> Node a -> a -> Render c ()
renderUniform uniformStorage node value = liftIO $ setUniform uniformStorage node value

setIndexedUniform :: (OfValueType a, Storable a) => UniformStorage d -> Node [a] -> Int -> a -> IO ()
setIndexedUniform UniformStorage
	{ uniformStorageBytes = bytes
	} (UniformNode Uniform
	{ uniformOffset = offset
	}) i value = do
	withForeignPtr bytes $ \ptr -> do
		pokeElemOff (ptr `plusPtr` offset) i value
setIndexedUniform _ _ _ _ = undefined

renderIndexedUniform :: (OfValueType a, Storable a) => UniformStorage d -> Node [a] -> Int -> a -> Render c ()
renderIndexedUniform uniformStorage node i value = liftIO $ setIndexedUniform uniformStorage node i value

renderUploadUniformStorage :: Context c d => UniformStorage d -> Render c ()
renderUploadUniformStorage UniformStorage
	{ uniformStorageBufferId = uniformBuffer
	, uniformStorageBytes = bytes
	, uniformStorageSize = size
	} = do
	bs <- liftIO $ B.unsafePackCStringLen (castPtr $ unsafeForeignPtrToPtr bytes, size)
	renderUploadUniformBuffer uniformBuffer bs
	liftIO $ touchForeignPtr bytes

renderUniformStorage :: Context c d => UniformStorage d -> Render c ()
renderUniformStorage UniformStorage
	{ uniformStorageSlot = slot
	, uniformStorageBufferId = uniformBuffer
	} = renderUniformBuffer slot uniformBuffer

sampler :: (OfValueType s, OfValueType c) => Int -> SamplerDimension -> SamplerNode s c
sampler slot dimension = withUndefined2 f where
	f s c = SamplerNode Sampler
		{ samplerSlot = slot
		, samplerDimension = dimension
		, samplerSampleType = valueType s
		, samplerCoordsType = valueType c
		}
	withUndefined2 :: (s -> c -> SamplerNode s c) -> SamplerNode s c
	withUndefined2 q = q undefined undefined

sample :: (OfValueType s, OfVectorType (v c), OfVectorType (v Int32)) => SamplerNode s (v c) -> Node (v c) -> Node s
sample s c = SampleNode
	{ sampleNodeSamplerNode = s
	, sampleNodeCoordsNode = c
	, sampleNodeOffsetNode = Nothing
	, sampleNodeLod = SampleNodeAutoLod
	}

sampleOffset :: (OfValueType s, OfVectorType (v c), OfVectorType (v Int32)) => SamplerNode s (v c) -> Node (v c) -> Node (v Int32) -> Node s
sampleOffset s c o = SampleNode
	{ sampleNodeSamplerNode = s
	, sampleNodeCoordsNode = c
	, sampleNodeOffsetNode = Just o
	, sampleNodeLod = SampleNodeAutoLod
	}

sampleLod :: (OfValueType s, OfVectorType (v c), OfVectorType (v Int32)) => SamplerNode s (v c) -> Node (v c) -> Node c -> Node s
sampleLod s c l = SampleNode
	{ sampleNodeSamplerNode = s
	, sampleNodeCoordsNode = c
	, sampleNodeOffsetNode = Nothing
	, sampleNodeLod = SampleNodeLod l
	}

sampleLodOffset :: (OfValueType s, OfVectorType (v c), OfVectorType (v Int32)) => SamplerNode s (v c) -> Node (v c) -> Node c -> Node (v Int32) -> Node s
sampleLodOffset s c l o = SampleNode
	{ sampleNodeSamplerNode = s
	, sampleNodeCoordsNode = c
	, sampleNodeOffsetNode = Just o
	, sampleNodeLod = SampleNodeLod l
	}

sampleBias :: (OfValueType s, OfVectorType (v c), OfVectorType (v Int32)) => SamplerNode s (v c) -> Node (v c) -> Node c -> Node s
sampleBias s c b = SampleNode
	{ sampleNodeSamplerNode = s
	, sampleNodeCoordsNode = c
	, sampleNodeOffsetNode = Nothing
	, sampleNodeLod = SampleNodeBiasLod b
	}

sampleBiasOffset :: (OfValueType s, OfVectorType (v c), OfVectorType (v Int32)) => SamplerNode s (v c) -> Node (v c) -> Node c -> Node (v Int32) -> Node s
sampleBiasOffset s c b o = SampleNode
	{ sampleNodeSamplerNode = s
	, sampleNodeCoordsNode = c
	, sampleNodeOffsetNode = Just o
	, sampleNodeLod = SampleNodeBiasLod b
	}

sampleGrad :: (OfValueType s, OfVectorType (v c), OfVectorType (v Int32)) => SamplerNode s (v c) -> Node (v c) -> Node (v c) -> Node (v c) -> Node s
sampleGrad s c gx gy = SampleNode
	{ sampleNodeSamplerNode = s
	, sampleNodeCoordsNode = c
	, sampleNodeOffsetNode = Nothing
	, sampleNodeLod = SampleNodeGradLod gx gy
	}

sampleGradOffset :: (OfValueType s, OfVectorType (v c), OfVectorType (v Int32)) => SamplerNode s (v c) -> Node (v c) -> Node (v c) -> Node (v c) -> Node (v Int32) -> Node s
sampleGradOffset s c gx gy o = SampleNode
	{ sampleNodeSamplerNode = s
	, sampleNodeCoordsNode = c
	, sampleNodeOffsetNode = Just o
	, sampleNodeLod = SampleNodeGradLod gx gy
	}

withState :: (State -> IO (State, a)) -> Program a
withState f = do
	stateVar <- ask
	liftIO $ do
		state <- readIORef stateVar
		(newState, result) <- f state
		writeIORef stateVar newState
		return result

temp :: OfValueType a => Node a -> Program (Node a)
temp = withState . tempInternal

tempInternal :: OfValueType a => Node a -> State -> IO (State, Node a)
tempInternal node state@State
	{ stateStage = stage
	, stateTemps = temps
	, stateTempsCount = tempsCount
	} = do
	if stage == EndStage then fail "failed to add temp after end of the program"
	else return ()
	return (state
		{ stateTemps = (Temp
			{ tempIndex = tempsCount
			, tempNode = node
			, tempStage = stage
			, tempType = nodeValueType node
			}) : temps
		, stateTempsCount = tempsCount + 1
		}, TempNode tempsCount)

rasterize :: Node Float4 -> Program () -> Program ()
rasterize positionNode pixelProgram = withState $ \state@State
	{ stateStage = stage
	, stateTargets = targets
	} -> do
	if stage /= VertexStage then fail $ show ("wrong stage to add pixel program", stage)
	else return ()
	let positionTarget = PositionTarget positionNode
	pixelStateVar <- newIORef state
		{ stateStage = PixelStage
		, stateTargets = positionTarget : targets
		}
	runReaderT pixelProgram pixelStateVar
	pixelState <- readIORef pixelStateVar
	return (pixelState
		{ stateStage = EndStage
		}, ())

colorTarget :: Int -> Node Float4 -> Program ()
colorTarget i colorNode = withState $ \state@State
	{ stateStage = stage
	, stateTargets = targets
	} -> do
	if stage /= PixelStage then fail $ "colorTarget can be used only in pixel program"
	else return ()
	let target = ColorTarget i colorNode
	return (state
		{ stateTargets = targets ++ [target]
		}, ())

dualColorTarget :: Node Float4 -> Node Float4 -> Program ()
dualColorTarget colorNode1 colorNode2 = withState $ \state@State
	{ stateStage = stage
	, stateTargets = targets
	} -> do
	if stage /= PixelStage then fail $ "dualColorTarget can be used only in pixel program"
	else return ()
	let target = DualColorTarget colorNode1 colorNode2
	return (state
		{ stateTargets = target : targets
		}, ())

depthTarget :: Node Float -> Program ()
depthTarget depthNode = withState $ \state@State
	{ stateStage = stage
	, stateTargets = targets
	} -> do
	if stage /= PixelStage then fail $ "depthTarget can be used only in pixel program"
	else return ()
	let target = DepthTarget depthNode
	return (state
		{ stateTargets = target : targets
		}, ())

constf :: Float -> Node Float
constf = cnst
const2f :: Float2 -> Node Float2
const2f = cnst
const3f :: Float3 -> Node Float3
const3f = cnst
const4f :: Float4 -> Node Float4
const4f = cnst

sampler1Df :: Int -> SamplerNode Float Float
sampler1Df slot = sampler slot Sampler1D
sampler1D2f :: Int -> SamplerNode Float2 Float
sampler1D2f slot = sampler slot Sampler1D
sampler1D3f :: Int -> SamplerNode Float3 Float
sampler1D3f slot = sampler slot Sampler1D
sampler1D4f :: Int -> SamplerNode Float4 Float
sampler1D4f slot = sampler slot Sampler1D

sampler2Df :: Int -> SamplerNode Float Float2
sampler2Df slot = sampler slot Sampler2D
sampler2D2f :: Int -> SamplerNode Float2 Float2
sampler2D2f slot = sampler slot Sampler2D
sampler2D3f :: Int -> SamplerNode Float3 Float2
sampler2D3f slot = sampler slot Sampler2D
sampler2D4f :: Int -> SamplerNode Float4 Float2
sampler2D4f slot = sampler slot Sampler2D

sampler3Df :: Int -> SamplerNode Float Float3
sampler3Df slot = sampler slot Sampler3D
sampler3D2f :: Int -> SamplerNode Float2 Float3
sampler3D2f slot = sampler slot Sampler3D
sampler3D3f :: Int -> SamplerNode Float3 Float3
sampler3D3f slot = sampler slot Sampler3D
sampler3D4f :: Int -> SamplerNode Float4 Float3
sampler3D4f slot = sampler slot Sampler3D

samplerCubef :: Int -> SamplerNode Float Float3
samplerCubef slot = sampler slot SamplerCube
samplerCube2f :: Int -> SamplerNode Float2 Float3
samplerCube2f slot = sampler slot SamplerCube
samplerCube3f :: Int -> SamplerNode Float3 Float3
samplerCube3f slot = sampler slot SamplerCube
samplerCube4f :: Int -> SamplerNode Float4 Float3
samplerCube4f slot = sampler slot SamplerCube

(!) :: (OfValueType a, OfValueType b, Integral b) => Node [a] -> Node b -> Node a
a ! b = IndexNode (nodeArrayValueType a) (nodeValueType b) a b

min_ :: OfValueType a => Node a -> Node a -> Node a
min_ a b = MinNode (nodeValueType a) a b

max_ :: OfValueType a => Node a -> Node a -> Node a
max_ a b = MaxNode (nodeValueType a) a b

clamp :: OfValueType a => Node a -> Node a -> Node a -> Node a
clamp a b c = ClampNode (nodeValueType a) a b c

lerp :: OfValueType a => Node a -> Node a -> Node a -> Node a
lerp a b c = LerpNode (nodeValueType a) a b c

equal_ :: OfValueType a => Node a -> Node a -> Node Bool
equal_ a b = EqualNode (nodeValueType a) a b

less_ :: OfValueType a => Node a -> Node a -> Node Bool
less_ a b = LessNode (nodeValueType a) a b

lessEqual_ :: OfValueType a => Node a -> Node a -> Node Bool
lessEqual_ a b = LessEqualNode (nodeValueType a) a b

if_ :: OfValueType a => Node Bool -> Node a -> Node a -> Node a
if_ c a b = IfNode (nodeValueType a) c a b

ddx :: OfValueType a => Node a -> Node a
ddx a = DdxNode (nodeValueType a) a

ddy :: OfValueType a => Node a -> Node a
ddy a = DdyNode (nodeValueType a) a

instanceId :: Node Word32
instanceId = InstanceIdNode

invSqrt :: (OfValueType a, Floating a) => Node a -> Node a
invSqrt a = InvSqrtNode (nodeValueType a) a

screenToTexture :: OfValueType a => Node a -> Node a
screenToTexture a = ScreenToTextureNode (nodeValueType a) a

normalizeSampledDepth :: Node Float -> Node Float
normalizeSampledDepth = NormalizeSampledDepthNode

fragCoord :: Node Float4
fragCoord = FragCoordNode
