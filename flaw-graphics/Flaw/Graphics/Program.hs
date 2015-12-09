{-|
Module: Flaw.Graphics.Program
Description: Shader program support.
License: MIT
-}

module Flaw.Graphics.Program
	( Program
	, AttributeFormat(..)
	, Normalization(..)
	, Node
	, cnst
	, constf, const2f, const3f, const4f
	, attribute
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
	, sample
	, temp
	, rasterize
	, colorTarget
	, depthTarget
	, (!)
	, min_, max_
	, ddx, ddy
	, instanceId
	) where

import Control.Monad.Reader
import qualified Data.ByteString.Unsafe as B
import Data.IORef
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import Foreign.Storable

import Flaw.Graphics
import Flaw.Graphics.Program.Internal
import Flaw.Math

cnst :: OfValueType a => a -> Node a
cnst value = ConstNode (valueType value) value

attribute :: OfAttributeType a => Int -> Int -> Int -> AttributeFormat a -> Program (Node a)
attribute slot offset divisor format = withState $ \state@State
	{ stateStage = stage
	} -> do
	if stage /= VertexStage then fail "attribute can only be defined in vertex program"
	else return ()
	let
		withUndefined :: (a -> Node a) -> Node a
		withUndefined q = q undefined
	tempInternal (withUndefined $ \u -> AttributeNode Attribute
		{ attributeSlot = slot
		, attributeOffset = offset
		, attributeDivisor = divisor
		, attributeType = attributeFormatToType format
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

uniform :: (OfValueType a, Storable a) => UniformBufferSlot -> IO (Node a)
uniform UniformBufferSlot
	{ uniformBufferSlotIndex = slot
	, uniformBufferSlotSizeRef = sizeRef
	} = withUndefined func where
	withUndefined :: (a -> IO (Node a)) -> IO (Node a)
	withUndefined f = f undefined
	func u = do
		bufferSize <- readIORef sizeRef
		let align = alignment u
		let alignedBufferSize = ((bufferSize + align - 1) `div` align) * align
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
	} = withUndefined func where
	withUndefined :: (a -> IO (Node [a])) -> IO (Node [a])
	withUndefined f = f undefined
	func u = do
		bufferSize <- readIORef sizeRef
		let align = alignment u
		let alignedBufferSize = ((bufferSize + align - 1) `div` align) * align
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
	let alignedSize = ((size + 15) `div` 16) * 16
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

sample :: (OfValueType s, OfValueType c) => SamplerNode s c -> Node c -> Node s
sample = SampleNode

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

rasterize :: Node Vec4f -> Program () -> Program ()
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

colorTarget :: Int -> Node Vec4f -> Program ()
colorTarget i colorNode = withState $ \state@State
	{ stateStage = stage
	, stateTargets = targets
	} -> do
	if stage /= PixelStage then fail $ "colorTarget can be used only in pixel program"
	else return ()
	let target = ColorTarget i colorNode
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
const2f :: Vec2f -> Node Vec2f
const2f = cnst
const3f :: Vec3f -> Node Vec3f
const3f = cnst
const4f :: Vec4f -> Node Vec4f
const4f = cnst

sampler1Df :: Int -> SamplerNode Float Float
sampler1Df slot = sampler slot Sampler1D
sampler1D2f :: Int -> SamplerNode Vec2f Float
sampler1D2f slot = sampler slot Sampler1D
sampler1D3f :: Int -> SamplerNode Vec3f Float
sampler1D3f slot = sampler slot Sampler1D
sampler1D4f :: Int -> SamplerNode Vec4f Float
sampler1D4f slot = sampler slot Sampler1D

sampler2Df :: Int -> SamplerNode Float Vec2f
sampler2Df slot = sampler slot Sampler2D
sampler2D2f :: Int -> SamplerNode Vec2f Vec2f
sampler2D2f slot = sampler slot Sampler2D
sampler2D3f :: Int -> SamplerNode Vec3f Vec2f
sampler2D3f slot = sampler slot Sampler2D
sampler2D4f :: Int -> SamplerNode Vec4f Vec2f
sampler2D4f slot = sampler slot Sampler2D

sampler3Df :: Int -> SamplerNode Float Vec3f
sampler3Df slot = sampler slot Sampler3D
sampler3D2f :: Int -> SamplerNode Vec2f Vec3f
sampler3D2f slot = sampler slot Sampler3D
sampler3D3f :: Int -> SamplerNode Vec3f Vec3f
sampler3D3f slot = sampler slot Sampler3D
sampler3D4f :: Int -> SamplerNode Vec4f Vec3f
sampler3D4f slot = sampler slot Sampler3D

samplerCubef :: Int -> SamplerNode Float Vec3f
samplerCubef slot = sampler slot SamplerCube
samplerCube2f :: Int -> SamplerNode Vec2f Vec3f
samplerCube2f slot = sampler slot SamplerCube
samplerCube3f :: Int -> SamplerNode Vec3f Vec3f
samplerCube3f slot = sampler slot SamplerCube
samplerCube4f :: Int -> SamplerNode Vec4f Vec3f
samplerCube4f slot = sampler slot SamplerCube

(!) :: (OfValueType a, OfValueType b, Integral b) => Node [a] -> Node b -> Node a
a ! b = IndexNode (nodeArrayValueType a) (nodeValueType b) a b

min_ :: OfValueType a => Node a -> Node a -> Node a
min_ a b = MinNode (nodeValueType a) a b

max_ :: OfValueType a => Node a -> Node a -> Node a
max_ a b = MaxNode (nodeValueType a) a b

ddx :: OfValueType a => Node a -> Node a
ddx a = DdxNode (nodeValueType a) a

ddy :: OfValueType a => Node a -> Node a
ddy a = DdyNode (nodeValueType a) a

instanceId :: Node Word
instanceId = InstanceIdNode
