{-|
Module: Flaw.Graphics.Program
Description: Shader program support.
License: MIT
-}

{-# LANGUAGE TemplateHaskell #-}

module Flaw.Graphics.Program
	( Program
	, AttributeFormat(..)
	, cnst
	, constf, const2f, const3f, const4f
	, attribute
	, uniform
	, uniformf
	, uniform1f, uniform2f, uniform3f, uniform4f
	, uniform1x1f, uniform1x2f, uniform1x3f, uniform1x4f
	, uniform2x1f, uniform2x2f, uniform2x3f, uniform2x4f
	, uniform3x1f, uniform3x2f, uniform3x3f, uniform3x4f
	, uniform4x1f, uniform4x2f, uniform4x3f, uniform4x4f
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
	) where

import Control.Monad.Reader
import Data.IORef
import Language.Haskell.TH

import Flaw.Graphics.Program.Internal
import Flaw.Math

withUndefined :: (a -> Node a) -> Node a
withUndefined q = q undefined

cnst :: OfValueType a => a -> Node a
cnst value = ConstNode (valueType value) value

attribute :: OfAttributeType a => Int -> Int -> Int -> AttributeFormat a -> Program (Node a)
attribute slot offset divisor format = withState $ \state@State
	{ stateStage = stage
	} -> do
	if stage /= VertexStage then fail "attribute can only be defined in vertex program"
	else return ()
	tempInternal (withUndefined $ \u -> AttributeNode Attribute
		{ attributeSlot = slot
		, attributeOffset = offset
		, attributeDivisor = divisor
		, attributeType = attributeFormatToType format
		, attributeValueType = valueType u
		}) state

uniform :: OfValueType a => Int -> Int -> Int -> Node a
uniform slot offset size = withUndefined f where
	f u = UniformNode Uniform
		{ uniformSlot = slot
		, uniformOffset = offset
		, uniformSize = size
		, uniformType = valueType u
		}

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

depthTarget :: Int -> Node Float -> Program ()
depthTarget index depthNode = withState $ \state@State
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

uniformf :: Int -> Int -> Int -> Node Float
uniformf = uniform

liftM concat $ forM ['1'..'4'] $ \d -> do
	let name = mkName $ "uniform" ++ [d, 'f']
	sigDec <- sigD name [t| Int -> Int -> Int -> Node $(appT (conT $ mkName $ "Vec" ++ [d]) (conT ''Float)) |]
	valDec <- valD (varP name) (normalB $ varE 'uniform) []
	return [sigDec, valDec]

liftM concat $ forM [(di, dj) | di <- ['1'..'4'], dj <- ['1'..'4'] ] $ \(di, dj) -> do
	let name = mkName $ "uniform" ++ [di, 'x', dj, 'f']
	sigDec <- sigD name [t| Int -> Int -> Int -> Node $(appT (conT $ mkName $ "Mat" ++ [di, 'x', dj]) (conT ''Float)) |]
	valDec <- valD (varP name) (normalB $ varE 'uniform) []
	return [sigDec, valDec]

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
