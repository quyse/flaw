{-|
Module: Flaw.Graphics.DirectX11.Internal
Description: Internals of graphics implementation for DirectX 11.
License: MIT
-}

{-# LANGUAGE GADTs, TypeFamilies #-}

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
import Data.Maybe
import Data.Monoid
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TLB

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
	, hlslAttributeAttributeType :: !AttributeType
	, hlslAttributeType :: !ProgramType
	}

-- | HLSL uniform.
data HlslUniform = HlslUniform
	{ hlslUniformName :: !T.Text
	, hlslUniformSlot :: !Int
	, hlslUniformOffset :: !Int
	, hlslUniformSize :: !Int
	, hlslUniformUniformType :: !UniformType
	, hlslUniformType :: !ProgramType
	}

-- | HLSL sampler.
data HlslSampler = HlslSampler
	{ hlslSamplerTextureName :: !T.Text
	, hlslSamplerSamplerStateName :: !T.Text
	, hlslSamplerSlot :: !Int
	, hlslSamplerDimension :: !ProgramSamplerDimension
	, hlslSamplerType :: !ProgramType
	}

-- | HLSL temp variable.
data HlslTemp = HlslTemp
	{ hlslTempName :: !T.Text
	-- | Line of code for temp variable.
	, hlslTempCode :: TLB.Builder
	-- | List of indices of temp nodes this temp depends on.
	, hlslTempDepends :: [Int]
	, hlslTempType :: !ProgramType
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
		-- | Map of temps by number.
	, stateTemps :: Map.Map Int HlslTemp
	}

withHlslState :: HlslGenerator -> (State -> IO (State, a)) -> IO a
withHlslState (HlslGenerator sr) f = do
	s0 <- readIORef sr
	(s1, r) <- f s0
	writeIORef sr s1
	return r

withUndefined :: (a -> IO (b, q a)) -> IO (b, q a)
withUndefined f = f undefined

instance ProgramGenerator HlslGenerator where

	data ProgramNode HlslGenerator s q where
		-- | Reference to variable with name.
		HlslVarNode :: T.Text -> ProgramNode HlslGenerator s q
		-- | Reference to temp variable with index and name.
		HlslTempNode :: Int -> T.Text -> ProgramNode HlslGenerator s q
		HlslBinaryNode
			:: ProgramNode HlslGenerator s a
			-> TLB.Builder
			-> ProgramNode HlslGenerator s b
			-> ProgramNode HlslGenerator s q
		HlslCall1Node
			:: TLB.Builder
			-> ProgramNode HlslGenerator s a
			-> ProgramNode HlslGenerator s q
		HlslCall2Node
			:: TLB.Builder
			-> ProgramNode HlslGenerator s a
			-> ProgramNode HlslGenerator s b
			-> ProgramNode HlslGenerator s q
		HlslCall3Node
			:: TLB.Builder
			-> ProgramNode HlslGenerator s a
			-> ProgramNode HlslGenerator s b
			-> ProgramNode HlslGenerator s c
			-> ProgramNode HlslGenerator s q
		HlslCall4Node
			:: TLB.Builder
			-> ProgramNode HlslGenerator s a
			-> ProgramNode HlslGenerator s b
			-> ProgramNode HlslGenerator s c
			-> ProgramNode HlslGenerator s d
			-> ProgramNode HlslGenerator s q

	data ProgramSamplerNode HlslGenerator s a b
		-- | Reference to sampler with slot, texture name and sampler name.
		= HlslSamplerNode !Int !T.Text !T.Text

	programRegisterAttribute g slot offset divisor aType = withHlslState g $ \s@State
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
				, hlslAttributeAttributeType = aType
				, hlslAttributeType = programType u
				}
			let node = HlslVarNode name
			return (s
				{ stateAttributes = Map.insert offset attribute attributes
				}, node)

	programRegisterUniform g slot offset size uType = withHlslState g $ \s@State
		{ stateUniforms = uniforms
		} -> do
		let key = (slot, offset)
		if Map.member key uniforms then fail $ show ("duplicated uniform", slot, offset)
		else return ()
		withUndefined $ \u -> do
			let name = T.pack $ "u" ++ show slot ++ "_" ++ show offset
			let uniform = HlslUniform
				{ hlslUniformName = name
				, hlslUniformSlot = slot
				, hlslUniformOffset = offset
				, hlslUniformSize = size
				, hlslUniformUniformType = uType
				, hlslUniformType = programType u
				}
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
			let node = HlslSamplerNode slot textureName samplerStateName
			return (s
				{ stateSamplers = Map.insert slot sampler samplers
				}, node)

	programRegisterTemp g forNode = withHlslState g $ \s@State
		{ stateTemps = temps
		} -> do
		withUndefined $ \u -> do
			let index = Map.size temps
			let name = T.pack $ "a" ++ show index
			let (code, depends) = compile s forNode
			let temp = HlslTemp
				{ hlslTempName = name
				, hlslTempCode = code
				, hlslTempDepends = depends
				, hlslTempType = programType u
				}
			let node = HlslTempNode index name
			return (s
				{ stateTemps = Map.insert index temp temps
				}, node)
{-
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

-- | Construct one line of code for given node.
compile :: State -> ProgramNode HlslGenerator s a -> (TLB.Builder, [Int])
compile state@State
	{ stateTemps = temps
	} node = case node of
	HlslVarNode name -> (TLB.fromText name, [])
	HlslTempNode index name -> let
		HlslTemp { hlslTempDepends = depends
		} = fromJust $ Map.lookup index temps
		in (TLB.fromText name, depends)
	HlslBinaryNode node1 op node2 -> let
		(code1, depends1) = compile state node1
		(code2, depends2) = compile state node2
		in (
			(TLB.singleton '(') `mappend`
			code1 `mappend`
			(TLB.singleton ')') `mappend`
			op `mappend`
			(TLB.singleton '(') `mappend`
			code2 `mappend`
			(TLB.singleton ')'), depends1 ++ depends2)
	HlslCall1Node fun node1 -> let
		(code1, depends1) = compile state node1
		in (fun `mappend`
			(TLB.singleton '(') `mappend`
			code1 `mappend`
			(TLB.singleton ')'), depends1)
	HlslCall2Node fun node1 node2 -> let
		(code1, depends1) = compile state node1
		(code2, depends2) = compile state node2
		in (fun `mappend`
			(TLB.singleton '(') `mappend`
			code1 `mappend`
			(TLB.singleton ',') `mappend`
			code2 `mappend`
			(TLB.singleton ')'), depends1 ++ depends2)
	HlslCall3Node fun node1 node2 node3 -> let
		(code1, depends1) = compile state node1
		(code2, depends2) = compile state node2
		(code3, depends3) = compile state node3
		in (fun `mappend`
			(TLB.singleton '(') `mappend`
			code1 `mappend`
			(TLB.singleton ',') `mappend`
			code2 `mappend`
			(TLB.singleton ',') `mappend`
			code3 `mappend`
			(TLB.singleton ')'), depends1 ++ depends2 ++ depends3)
	HlslCall4Node fun node1 node2 node3 node4 -> let
		(code1, depends1) = compile state node1
		(code2, depends2) = compile state node2
		(code3, depends3) = compile state node3
		(code4, depends4) = compile state node4
		in (fun `mappend`
			(TLB.singleton '(') `mappend`
			code1 `mappend`
			(TLB.singleton ',') `mappend`
			code2 `mappend`
			(TLB.singleton ',') `mappend`
			code3 `mappend`
			(TLB.singleton ',') `mappend`
			code4 `mappend`
			(TLB.singleton ')'), depends1 ++ depends2 ++ depends3 ++ depends4)

-- | Generate shader programs in HLSL.
generateHlsl :: (HlslGenerator -> IO ()) -> IO HlslProgram
generateHlsl p = do
	varState <- newIORef State
		{ stateAttributes = Map.empty
		, stateUniforms = Map.empty
		, stateSamplers = Map.empty
		, stateTemps = Map.empty
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
