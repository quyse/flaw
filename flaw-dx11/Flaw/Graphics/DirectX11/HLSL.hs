{-|
Module: Flaw.Graphics.DirectX11.Internal
Description: Internals of graphics implementation for DirectX 11.
License: MIT
-}

{-# LANGUAGE TypeFamilies #-}

module Flaw.Graphics.DirectX11.HLSL
	( HlslGenerator(..)
	, HlslAttribute(..)
	, HlslUniform(..)
	, HlslShader(..)
	, HlslProgram(..)
	, generateHlsl
	) where

import Data.Char
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Flaw.Graphics.Program.Internal

-- | HLSL program generator.
newtype HlslGenerator = HlslGenerator (IORef State)

-- | Hlsl attribute.
data HlslAttribute = HlslAttribute
	{ hlslAttributeName :: !T.Text
	, hlslAttributeSemantic :: !T.Text
	, hlslAttributeSlot :: !Int
	, hlslAttributeOffset :: !Int
	, hlslAttributeDivisor :: !Int
	, hlslAttributeAttributeType :: !ProgramAttributeType
	, hlslAttributeType :: !ProgramType
	}

-- | HLSL uniform.
data HlslUniform = HlslUniform
	{ hlslUniformName :: !T.Text
	, hlslUniformSlot :: !Int
	, hlslUniformOffset :: !Int
	, hlslUniformSize :: !Int
	, hlslUniformType :: !ProgramType
	}

-- | HLSL sampler.
data HlslSampler = HlslSampler
	{ hlslSamplerTextureName :: !T.Text
	, hlslSamplerSamplerStateName :: !T.Text
	, hlslSamplerSlot :: !Int
	, hlslSamplerDimension :: !Int
	, hlslSamplerType :: !ProgramType
	}

-- | HLSL shader.
data HlslShader = HlslShader
	{ hlslShaderSource :: T.Text
	, hlslShaderEntryPoint :: T.Text
	, hlslShaderTarget :: T.Text
	, hlslShaderUniforms :: [HlslUniform]
	, hlslShaderSamplers :: [HlslSampler]
	}

-- | HLSL program.
data HlslProgram
	= HlslVertexPixelProgram [HlslAttribute] HlslShader HlslShader

-- | State of HLSL generator.
data State = State
	{
		-- | Map of attributes by offset.
		stateAttributes :: Map.Map Int HlslAttribute
		-- | Map of uniforms by (slot, offset).
	, stateUniforms :: Map.Map (Int, Int) HlslUniform
		-- | Map of samplers by slot.
	, stateSamplers :: Map.Map Int HlslSampler
	}

withHlslState :: HlslGenerator -> (State -> IO (State, a)) -> IO a
withHlslState (HlslGenerator sr) f = do
	s0 <- readIORef sr
	(s1, r) <- f s0
	writeIORef sr s1
	return r

withUndefined :: (a -> IO (b, ProgramNode HlslGenerator s a)) -> IO (b, ProgramNode HlslGenerator s a)
withUndefined f = f undefined

instance ProgramGenerator HlslGenerator where

	data ProgramNode HlslGenerator s a =
		-- | Reference to variable with name.
			HlslVarNode T.Text
		-- | Reference to sampler with texture name and sampler name.
		| HlslSamplerNode T.Text T.Text

	programRegisterAttribute g slot offset divisor attributeType = withHlslState g $ \s@State
		{ stateAttributes = attributes
		} -> do
		if Map.member offset attributes then fail $ show ("attribute already registered", offset)
		else return ()
		withUndefined $ \u -> do
			let name = T.pack $ "a" ++ show offset
			let attribute = HlslAttribute
				{ hlslAttributeName = name
				, hlslAttributeSemantic = hlslSemanticString offset
				, hlslAttributeSlot = slot
				, hlslAttributeOffset = offset
				, hlslAttributeDivisor = divisor
				, hlslAttributeAttributeType = attributeType
				, hlslAttributeType = programType u
				}
			let node = HlslVarNode name
			return (s
				{ stateAttributes = Map.insert offset attribute attributes
				}, node)

	programRegisterUniform g slot offset size = withHlslState g $ \s@State
		{ stateUniforms = uniforms
		} -> do
		let key = (slot, offset)
		if Map.member key uniforms then fail $ show ("duplicated uniform", slot, offset)
		else return ()
		withUndefined $ \u -> do
			let name = T.pack $ "u" ++ show slot ++ "_" ++ show offset
			let uniform = HlslUniform name slot offset size $ programType u
			let node = HlslVarNode name
			return (s
				{ stateUniforms = Map.insert key uniform uniforms
				}, node)

	programRegisterSampler g slot dimension = withHlslState g $ \s@State
		{ stateSamplers = samplers
		} -> do
		if Map.member slot samplers then fail $ show ("duplicated sampler", slot)
		else return ()
		withUndefined $ \u -> do
			let textureName = T.pack $ "t" ++ show slot
			let samplerStateName = T.pack $ "ss" ++ show slot
			let sampler = HlslSampler
				{ hlslSamplerTextureName = textureName
				, hlslSamplerSamplerStateName = samplerStateName
				, hlslSamplerSlot = slot
				, hlslSamplerDimension = dimension
				, hlslSamplerType = programType u
				}
			let node = HlslSamplerNode textureName samplerStateName
			return (s
				{ stateSamplers = Map.insert slot sampler samplers
				}, node)

{-
	programRegisterValue
	programInterpolate
	programNodeConst
	programNodeCombineVec2
	programNodeCombineVec3
	programNodeCombineVec4
	programNodeAdd
	programNodeSubtract
	programNodeMultiply
	programNodeDivide
	programNodeRecip
	programNodeNegate
	programNodeAbs
	programNodeSignum
	programNodePi
	programNodeExp
	programNodeSqrt
	programNodeLog
	programNodePow
	programNodeLogBase
	programNodeSin
	programNodeTan
	programNodeCos
	programNodeAsin
	programNodeAtan
	programNodeAcos
	programNodeSinh
	programNodeTanh
	programNodeCosh
	programNodeAsinh
	programNodeAtanh
	programNodeAcosh
	programNodeMul
	programNodeDot
	programNodeInstanceID
	programNodeComponent
	programNodeSwizzle
-}

-- | Generate shader programs in HLSL.
generateHlsl :: (HlslGenerator -> IO ()) -> IO HlslProgram
generateHlsl p = do
	varState <- newIORef State
		{ stateAttributes = Map.empty
		, stateUniforms = Map.empty
		, stateSamplers = Map.empty
		}
	let g = HlslGenerator varState
	p g
	-- TODO
	return undefined

-- | Calculate semantic string for a number.
hlslSemanticString :: Int -> T.Text
hlslSemanticString = T.pack . (str 8) where
	str :: Int -> Int -> String
	str 0 _ = ""
	str n i = (chr (ord 'A' + i `rem` 26)) : str (n - 1) (i `div` 26)
