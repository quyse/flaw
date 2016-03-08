{-|
Module: Flaw.Graphics.GLSL
Description: GLSL generator for OpenGL/WebGL graphics.
License: MIT
-}

{-# LANGUAGE DeriveGeneric, GADTs, OverloadedStrings, TypeFamilies #-}

module Flaw.Graphics.GLSL
	( GlslConfig(..)
	, GlslAttribute(..)
	, GlslUniformBlock(..)
	, GlslUniform(..)
	, GlslSampler(..)
	, GlslFragmentTarget(..)
	, GlslStage(..)
	, GlslProgram(..)
	, glslGenerateProgram
	) where

import Data.List
import Data.Monoid
import qualified Data.Serialize as S
import Data.Serialize.Text()
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder
import GHC.Generics(Generic)

import Flaw.Graphics.Program.Internal
import qualified Flaw.Graphics.Program.SL as SL

-- | GLSL config for various versions of GLSL.
data GlslConfig = GlslConfig
	{
	-- | GLSL version.
	  glslConfigVersion :: Maybe Int
	-- | Replace integer types with float types for attributes.
	, glslConfigForceFloatAttributes :: !Bool
	-- | Replace unsigned types with signed types.
	, glslConfigUnsignedUnsupported :: !Bool
	-- | Use uniform blocks (instead of separate uniforms).
	, glslConfigUniformBlocks :: !Bool
	-- | Use "in" and "out" keywords instead of "attribute" and "varying", declare fragment targets.
	, glslConfigInOutSyntax :: !Bool
	-- | Use dimension specifiers in texture sampling functions (like texture2D vs texture).
	, glslConfigTextureSampleDimensionSpecifier :: !Bool
	}

data GlslAttribute = GlslAttribute
	{ glslAttributeName :: !T.Text
	, glslAttributeInfo :: Attribute
	} deriving (Show, Generic)
instance S.Serialize GlslAttribute

-- | Information for binding uniform block to slot.
-- It might be multiple uniform blocks bound to a single slot
-- (for example, if uniforms from this slot are used in multiple shader stages).
data GlslUniformBlock = GlslUniformBlock
	{ glslUniformBlockName :: !T.Text
	, glslUniformBlockSlot :: !Int
	} deriving (Show, Generic)
instance S.Serialize GlslUniformBlock

-- | Uniform used by shader.
data GlslUniform = GlslUniform
	{ glslUniformName :: !T.Text
	, glslUniformInfo :: Uniform
	} deriving (Show, Generic)
instance S.Serialize GlslUniform

-- | Sampler used by shader.
data GlslSampler = GlslSampler
	{ glslSamplerName :: !T.Text
	, glslSamplerInfo :: Sampler
	} deriving (Show, Generic)
instance S.Serialize GlslSampler

-- | Fragment target outputted by shader.
data GlslFragmentTarget
	= GlslFragmentTarget
	{ glslFragmentTargetName :: !T.Text
	, glslFragmentTargetIndex :: !Int
	}
	| GlslDualFragmentTarget
	{ glslFragmentTargetName0 :: !T.Text
	, glslFragmentTargetName1 :: !T.Text
	}
	deriving (Show, Generic)
instance S.Serialize GlslFragmentTarget

data GlslStage
	= GlslVertexStage
	| GlslFragmentStage
	deriving (Show, Generic)
instance S.Serialize GlslStage

-- | GLSL program.
data GlslProgram = GlslProgram
	{ glslProgramAttributes :: [GlslAttribute]
	, glslProgramUniformBlocks :: [GlslUniformBlock]
	, glslProgramUniforms :: [GlslUniform]
	, glslProgramSamplers :: [GlslSampler]
	, glslProgramFragmentTargets :: [GlslFragmentTarget]
	, glslProgramShaders :: [(GlslStage, T.Text)]
	} deriving (Show, Generic)
instance S.Serialize GlslProgram

-- | Generate shader programs in GLSL.
glslGenerateProgram :: GlslConfig -> State -> GlslProgram
glslGenerateProgram GlslConfig
	{ glslConfigVersion = configVersion
	, glslConfigForceFloatAttributes = configForceFloatAttributes
	, glslConfigUnsignedUnsupported = configUnsignedUnsupported
	, glslConfigUniformBlocks = configUniformBlocks
	, glslConfigInOutSyntax = configInOutSyntax
	, glslConfigTextureSampleDimensionSpecifier = configTextureSampleDimensionSpecifier
	} state = program where
	-- generate program information
	SL.ProgramInfo shaders = SL.programInfo state

	-- generate shaders and unite them
	program = foldr mergeProgram emptyProgram shaders
	mergeProgram shader@(stage, _shaderInfo) GlslProgram
		{ glslProgramAttributes = pas
		, glslProgramUniformBlocks = pubs
		, glslProgramUniforms = pus
		, glslProgramSamplers = pss
		, glslProgramFragmentTargets = pts
		, glslProgramShaders = ps
		} = GlslProgram
		{ glslProgramAttributes = as ++ pas
		, glslProgramUniformBlocks = ubs ++ pubs
		, glslProgramUniforms = us ++ pus
		, glslProgramSamplers = ss ++ pss
		, glslProgramFragmentTargets = ts ++ pts
		, glslProgramShaders = (glslStage, s) : ps
		} where
		(s, as, ubs, us, ss, ts) = generateShader shader
		glslStage = case stage of
			VertexStage -> GlslVertexStage
			PixelStage -> GlslFragmentStage
			_ -> error "wrong stage"
	emptyProgram = GlslProgram
		{ glslProgramAttributes = []
		, glslProgramUniformBlocks = []
		, glslProgramUniforms = []
		, glslProgramSamplers = []
		, glslProgramFragmentTargets = []
		, glslProgramShaders = []
		}

	-- function to generate shader source and lists of resources
	generateShader :: (Stage, SL.ShaderInfo) -> (T.Text, [GlslAttribute], [GlslUniformBlock], [GlslUniform], [GlslSampler], [GlslFragmentTarget])
	generateShader (stage, SL.ShaderInfo
		{ SL.shaderTemps = temps
		, SL.shaderAttributes = attributes
		, SL.shaderUniforms = uniforms
		, SL.shaderSamplers = samplers
		, SL.shaderTargets = targets
		}) = (TL.toStrict $ toLazyText source, resAttributes, resUniformBlocks, resUniforms, resSamplers, resFragmentTargets) where

		-- GLSL attributes
		resAttributes = map resAttribute attributes
		resAttribute attribute@Attribute
			{ attributeSlot = slot
			, attributeOffset = offset
			} = GlslAttribute
			{ glslAttributeName = TL.toStrict $ toLazyText $ attributeName slot offset
			, glslAttributeInfo = attribute
			}

		-- GLSL uniform blocks
		resUniformBlocks = if configUniformBlocks then map resUniformBlock uniformBlocks else []
		resUniformBlock us = GlslUniformBlock
			{ glslUniformBlockName = TL.toStrict $ toLazyText $ uniformBlockName slot
			, glslUniformBlockSlot = slot
			} where slot = uniformSlot $ head us

		-- GLSL uniforms
		resUniforms = if configUniformBlocks then [] else map resUniform uniforms
		resUniform uniform@Uniform
			{ uniformSlot = slot
			, uniformOffset = offset
			} = GlslUniform
			{ glslUniformName = TL.toStrict $ toLazyText $ uniformName slot offset
			, glslUniformInfo = uniform
			}

		-- GLSL samplers
		resSamplers = map resSampler samplers
		resSampler sampler@Sampler
			{ samplerSlot = slot
			} = GlslSampler
			{ glslSamplerName = TL.toStrict $ toLazyText $ samplerName slot
			, glslSamplerInfo = sampler
			}

		-- GLSL fragment targets
		resFragmentTargets = if configInOutSyntax then concatMap resFragmentTarget targets else []
		resFragmentTarget target = case target of
			PositionTarget _ -> []
			ColorTarget slot _ -> [GlslFragmentTarget
				{ glslFragmentTargetName = TL.toStrict $ toLazyText $ targetColorName slot
				, glslFragmentTargetIndex = slot
				}]
			DualColorTarget _ _ -> [GlslDualFragmentTarget
				{ glslFragmentTargetName0 = TL.toStrict $ toLazyText $ targetColorName 0
				, glslFragmentTargetName1 = TL.toStrict $ toLazyText $ targetColorName 1
				}]
			DepthTarget _ -> []

		-- source
		source = headerSource <> attributesSource <> inInterpolantsSource <> outInterpolantsSource
			<> (if configUniformBlocks then uniformBlocksSource else uniformsSource)
			<> samplersSource <> targetDeclsSource <> codeSource

		-- header source
		headerSource = versionSource <> "#ifdef GL_ES\nprecision highp float;\n#endif\n"
		versionSource = case configVersion of
			Just version -> "#version " <> fromString (show version) <> "\n"
			Nothing -> mempty

		-- attributes source
		attributesSource = foldr (mappend . attributeDefinitionSource) mempty attributes
		attributeDefinitionSource Attribute
			{ attributeSlot = slot
			, attributeOffset = offset
			, attributeValueType = t
			} = (if configInOutSyntax then "in " else "attribute ") <> attributeValueTypeSource t <> " " <> attributeName slot offset <> ";\n"

		-- in-interpolants source
		-- in-interpolants are temps used during this stage, but defined at another stage
		inInterpolantsSource = foldr (mappend . inInterpolantSource) mempty $ filter (\temp -> tempStage temp /= stage) temps
		inInterpolantSource Temp
			{ tempIndex = i
			, tempType = t
			} = (if configInOutSyntax then "in " else "varying ") <> valueTypeSource t <> " " <> interpolantName i <> ";\n"

		-- out-interpolants are temps defined at this stage, but used by some other stage
		outInterpolants = filter (\temp -> tempStage temp == stage && tempUsedByOtherStage (tempIndex temp)) temps

		-- out-interpolants source
		outInterpolantsSource = foldr (mappend . outInterpolantSource) mempty outInterpolants
		outInterpolantSource Temp
			{ tempIndex = i
			, tempType = t
			} = (if configInOutSyntax then "out " else "varying ") <> valueTypeSource t <> " " <> interpolantName i <> ";\n"
		otherShaderInfos = concatMap (\(otherStage, otherShaderInfo) -> if otherStage == stage then [] else [otherShaderInfo]) shaders
		tempUsedByOtherStage i = any (elem i . map tempIndex . SL.shaderTemps) otherShaderInfos

		-- uniform blocks source
		uniformBlocks = groupBy eqUniformBySlot $ sortBy compareUniformBySlot uniforms
		uniformBlocksSource = foldr (mappend . uniformBlockSource) mempty uniformBlocks
		uniformBlockSource blockUniforms = blockHeader <> blockSource <> blockFooter where
			blockHeader = "layout(std140) uniform " <> uniformBlockName (uniformSlot $ head blockUniforms) <> "\n{\n"
			blockFooter = "};\n"
			blockSource = foldr (mappend . uniformInBlockSource) mempty $ addBlockGaps blockUniforms 0
			uniformInBlockSource u = "\t" <> uniformDefinitionSource u
			addBlockGaps ua@(u:us) offset
				| advance == 0 = u : addBlockGaps us (offset + valueTypeScalarsCount (uniformType u) * count * 4)
				| advance `mod` 4 == 0 && advance >= 4 = Uniform
					{ uniformSlot = uniformSlot u
					, uniformOffset = offset
					, uniformSize = 0
					, uniformType = case cappedAdvance `div` 4 of
						1 -> ScalarValueType ScalarFloat
						2 -> VectorValueType Dimension2 ScalarFloat
						3 -> VectorValueType Dimension3 ScalarFloat
						4 -> VectorValueType Dimension4 ScalarFloat
						_ -> undefined
					} : addBlockGaps ua (offset + cappedAdvance)
				| otherwise = error "wrong uniform offset"
				where
					advance = uniformOffset u - offset
					cappedAdvance = min advance 16
					size = uniformSize u
					count = if size > 0 then size else 1
			addBlockGaps [] _ = []
		eqUniformBySlot a b = uniformSlot a == uniformSlot b
		compareUniformBySlot a b = compare (uniformSlot a) (uniformSlot b)

		-- uniforms source
		uniformsSource = foldr (mappend . uniformSource) mempty uniforms
		uniformSource uniform = "uniform " <> uniformDefinitionSource uniform

		-- helper function for uniform
		uniformDefinitionSource Uniform
			{ uniformSlot = slot
			, uniformOffset = offset
			, uniformSize = size
			, uniformType = t
			} = valueTypeSource t <> " " <> uniformName slot offset <> (if size > 0 then "[" <> fromString (show size) <> "]" else mempty) <> ";\n"

		-- samplers source
		samplersSource = foldr (mappend . samplerSource) mempty samplers
		samplerSource Sampler
			{ samplerSlot = slot
			, samplerDimension = dimension
			, samplerSampleType = sampleType
			} = "uniform " <> typeSource <> samplerDimensionSource <> " " <> samplerName slot <> ";\n" where
			typeSource = case sampleType of
				ScalarValueType st -> scalarShortTypeSource st
				VectorValueType _dim st -> scalarShortTypeSource st
				MatrixValueType _dim1 _dim2 st -> scalarShortTypeSource st
			samplerDimensionSource = case dimension of
				Sampler1D -> "sampler1D"
				Sampler2D -> "sampler2D"
				Sampler3D -> "sampler3D"
				SamplerCube -> "samplerCube"

		-- target decls source
		targetDeclsSource = if configInOutSyntax then foldr (mappend . targetDeclSource) mempty targets else mempty
		targetDeclSource target = case target of
			PositionTarget _ -> mempty
			ColorTarget slot _ -> "out " <> valueTypeSource (VectorValueType Dimension4 ScalarFloat) <> " " <> targetColorName slot <> ";\n"
			DualColorTarget a b -> targetDeclSource (ColorTarget 0 a) <> targetDeclSource (ColorTarget 1 b)
			DepthTarget _ -> mempty

		-- code source
		codeSource = "void main()\n{\n" <> tempsSource <> outInterpolantsAssignmentsSource <> targetsSource <> "}\n"

		-- definitions of temp variables
		tempsSource = foldr (mappend . tempSource) mempty temps
		tempSource Temp
			{ tempIndex = i
			, tempNode = node
			, tempStage = ts
			, tempType = t
			} = "\t" <> valueTypeSource t <> " " <> tempName i <> " = " <> (if ts == stage then tempNodeSource else interpolantName i) <> ";\n" where
			tempNodeSource = nodeSource $ case node of
				-- cast attribute node back to non-float type if needed
				AttributeNode _ -> if configForceFloatAttributes && t /= forceFloatType t then CastNode (forceFloatType t) t node else node
				_ -> node

		-- assignments to out-interpolants
		outInterpolantsAssignmentsSource = foldr (mappend . outInterpolantAssignmentSource) mempty outInterpolants
		outInterpolantAssignmentSource Temp
			{ tempIndex = i
			} = "\t" <> interpolantName i <> " = " <> tempName i <> ";\n"

		-- outputting targets
		targetsSource = foldr (mappend . targetSource) mempty targets
		targetSource target = case target of
			PositionTarget node -> "\t" <> targetPositionName <> " = " <> nodeSource node <> ";\n"
			ColorTarget slot node -> "\t" <> targetColorName slot <> " = " <> nodeSource node <> ";\n"
			DualColorTarget nodeA nodeB -> targetSource (ColorTarget 0 nodeA) <> targetSource (ColorTarget 1 nodeB)
			DepthTarget node -> "\t" <> targetDepthName <> " = " <> nodeSource node <> ";\n"

		-- helper functions

		attributeName :: Int -> Int -> Builder
		attributeName slot offset = "a" <> fromString (show slot) <> "_" <> fromString (show offset)

		interpolantName :: Int -> Builder
		interpolantName i = "i" <> fromString (show i)

		uniformBlockName :: Int -> Builder
		uniformBlockName slot = uniformBlockPrefix <> fromString (show slot)

		uniformName :: Int -> Int -> Builder
		uniformName slot offset = uniformPrefix <> fromString (show slot) <> "_" <> fromString (show offset)

		samplerName :: Int -> Builder
		samplerName slot = samplerPrefix <> fromString (show slot)

		tempName :: Int -> Builder
		tempName i = "_" <> fromString (show i)

		targetPositionName :: Builder
		targetPositionName = "gl_Position"

		targetColorName :: Int -> Builder
		targetColorName i
			| configInOutSyntax = "r" <> fromString (show i)
			| i == 0 = "gl_FragColor"
			| otherwise = "gl_FragData[" <> fromString (show i) <> "]"

		targetDepthName :: Builder
		targetDepthName = "gl_FragDepth"

		valueTypeSource :: ValueType -> Builder
		valueTypeSource vt = case vt of
			ScalarValueType st -> scalarTypeSource st
			VectorValueType d st -> scalarShortTypeSource st <> "vec" <> dimensionSource d
			MatrixValueType d1 d2 st ->
				if d1 == d2 then
					scalarShortTypeSource st <> "mat" <> dimensionSource d1
				else
					scalarShortTypeSource st <> "mat" <> dimensionSource d1 <> "x" <> dimensionSource d2

		-- | Special version of valueTypeSource for attributes. Forces use of float type if needed.
		attributeValueTypeSource :: ValueType -> Builder
		attributeValueTypeSource = if configForceFloatAttributes then valueTypeSource . forceFloatType else valueTypeSource

		forceFloatType :: ValueType -> ValueType
		forceFloatType vt = case vt of
			ScalarValueType st -> ScalarValueType $ forceFloatScalarType st
			VectorValueType d st -> VectorValueType d $ forceFloatScalarType st
			MatrixValueType d1 d2 st -> MatrixValueType d1 d2 $ forceFloatScalarType st

		forceFloatScalarType :: ScalarType -> ScalarType
		forceFloatScalarType st = case st of
			ScalarFloat -> ScalarFloat
			ScalarDouble -> ScalarDouble
			ScalarInt -> ScalarFloat
			ScalarUint -> ScalarFloat
			ScalarBool -> ScalarFloat

		scalarTypeSource :: ScalarType -> Builder
		scalarTypeSource st = case st of
			ScalarFloat -> "float"
			ScalarDouble -> "double"
			ScalarInt -> "int"
			ScalarUint -> if configUnsignedUnsupported then "int" else "uint"
			ScalarBool -> "bool"

		scalarShortTypeSource :: ScalarType -> Builder
		scalarShortTypeSource st = case st of
			ScalarFloat -> ""
			ScalarDouble -> "d"
			ScalarInt -> "i"
			ScalarUint -> if configUnsignedUnsupported then "i" else "u"
			ScalarBool -> "b"

		dimensionSource :: Dimension -> Builder
		dimensionSource d = case d of
			Dimension1 -> "1"
			Dimension2 -> "2"
			Dimension3 -> "3"
			Dimension4 -> "4"

		uniformBlockPrefix :: Builder
		uniformBlockPrefix = case stage of
			VertexStage -> "UBv"
			PixelStage -> "UBp"
			_ -> error "wrong stage"

		uniformPrefix :: Builder
		uniformPrefix = case stage of
			VertexStage -> "uv"
			PixelStage -> "up"
			_ -> error "wrong stage"

		samplerPrefix :: Builder
		samplerPrefix = case stage of
			VertexStage -> "sv"
			PixelStage -> "sp"
			_ -> error "wrong stage"

		nodeSource :: Node a -> Builder
		nodeSource node = case node of
			AttributeNode Attribute
				{ attributeSlot = slot
				, attributeOffset = offset
				} -> attributeName slot offset
			UniformNode Uniform
				{ uniformSlot = slot
				, uniformOffset = offset
				} -> uniformName slot offset
			TempNode i -> tempName i
			ConstNode t v -> let
				s = valueToShowList v
				content = case t of
					ScalarValueType _ -> head s
					VectorValueType _ _ -> intercalate ", " s
					MatrixValueType {} -> intercalate ", " s
				in valueTypeSource t <> "(" <> fromString content <> ")"
			IndexNode _ _ a b -> "(" <> nodeSource a <> ")[" <> nodeSource b <> "]"
			AddNode _ a b -> binaryOpSource "+" a b
			SubtractNode _ a b -> binaryOpSource "-" a b
			MultiplyNode _ a b -> binaryOpSource "*" a b
			DivideNode _ a b -> binaryOpSource "/" a b
			RecipNode _ a -> func1Source "rcp" a
			NegateNode _ a -> "-(" <> nodeSource a <> ")"
			AbsNode _ a -> func1Source "abs" a
			SignumNode _ a -> func1Source "sign" a
			MinNode _ a b -> func2Source "min" a b
			MaxNode _ a b -> func2Source "max" a b
			EqualNode _ a b -> binaryOpSource "==" a b
			LessNode _ a b -> binaryOpSource "<" a b
			LessEqualNode _ a b -> binaryOpSource "<=" a b
			IfNode _ c a b -> "(" <> nodeSource c <> ") ? (" <> nodeSource a <> ") : (" <> nodeSource b <> ")"
			PiNode t -> let
				typedPi :: Floating a => Node a -> a
				typedPi _ = pi
				in nodeSource $ ConstNode t $ typedPi node
			ExpNode _ a -> func1Source "exp" a
			SqrtNode _ a -> func1Source "sqrt" a
			LogNode _ a -> func1Source "log" a
			PowNode _ a b -> func2Source "pow" a b
			LogBaseNode t a b -> nodeSource $ DivideNode t (LogNode t a) (LogNode t b)
			SinNode _ a -> func1Source "sin" a
			TanNode _ a -> func1Source "tan" a
			CosNode _ a -> func1Source "tan" a
			AsinNode _ a -> func1Source "asin" a
			AtanNode _ a -> func1Source "atan" a
			AcosNode _ a -> func1Source "acos" a
			SinhNode _ a -> func1Source "sinh" a
			TanhNode _ a -> func1Source "tanh" a
			CoshNode _ a -> func1Source "cosh" a
			AsinhNode _ a -> func1Source "asinh" a
			AtanhNode _ a -> func1Source "atanh" a
			AcoshNode _ a -> func1Source "acosh" a
			MulNode _ _ _ a b -> binaryOpSource "*" a b
			DotNode _ _ a b -> func2Source "dot" a b
			CrossNode _ a b -> func2Source "cross" a b
			NormNode _ _ a -> func1Source "length" a
			Norm2Node _ _ a -> func1Source "length2" a
			NormalizeNode _ a -> func1Source "normalize" a
			DdxNode _ a -> func1Source "dFdx" a
			DdyNode _ a -> func1Source "dFdy" a
			InstanceIdNode -> "uint(gl_InstanceID)"
			ComponentNode _ _ c a -> "(" <> nodeSource a <> ")." <> singleton c
			SwizzleNode _ _ s a ->  "(" <> nodeSource a <> ")." <> fromString s
			SampleNode
				{ sampleNodeSamplerNode = SamplerNode Sampler
					{ samplerSlot = slot
					, samplerSampleType = sampleType
					, samplerCoordsType = coordsType
					}
				, sampleNodeCoordsNode = c
				, sampleNodeOffsetNode = mo
				, sampleNodeLod = ll
				} -> let
				sc = samplerName slot <> ", " <> nodeSource c
				coordsDim = if configTextureSampleDimensionSpecifier then case coordsType of
					ScalarValueType _ -> "1D"
					VectorValueType dim _ -> case dim of
						Dimension1 -> "1D"
						Dimension2 -> "2D"
						Dimension3 -> "3D"
						Dimension4 -> "4D"
					MatrixValueType {} -> error "invalid coords type"
					else mempty
				f = case mo of
					Nothing -> case ll of
						SampleNodeAutoLod -> "texture" <> coordsDim <> "(" <> sc <> ")"
						SampleNodeLod l -> "texture" <> coordsDim <> "Lod(" <> sc <> ", " <> nodeSource l <> ")"
						SampleNodeBiasLod b -> "texture" <> coordsDim <> "Bias(" <> sc <> ", " <> nodeSource b <> ")"
						SampleNodeGradLod gx gy -> "texture" <> coordsDim <> "Grad(" <> sc <> ", " <> nodeSource gx <> ", " <> nodeSource gy <> ")"
					Just o -> case ll of
						SampleNodeAutoLod -> "texture" <> coordsDim <> "Offset(" <> sc <> ", " <> nodeSource o <> ")"
						SampleNodeLod l -> "texture" <> coordsDim <> "LodOffset(" <> sc <> ", " <> nodeSource l <> ", " <> nodeSource o <> ")"
						SampleNodeBiasLod b -> "texture" <> coordsDim <> "Bias(" <> sc <> ", " <> nodeSource b <> ", " <> nodeSource o <> ")"
						SampleNodeGradLod gx gy -> "texture" <> coordsDim <> "Grad(" <> sc <> ", " <> nodeSource gx <> ", " <> nodeSource gy <> ", " <> nodeSource o <> ")"
				in f <> case sampleType of
					ScalarValueType _ -> ".x"
					VectorValueType dim _ -> case dim of
						Dimension1 -> ".x"
						Dimension2 -> ".xy"
						Dimension3 -> ".xyz"
						Dimension4 -> mempty
					MatrixValueType {} -> mempty
			CastNode _ t a -> valueTypeSource t <> "(" <> nodeSource a <> ")"
			Combine2VecNode _ _ t a b -> func2Source (valueTypeSource t) a b
			Combine3VecNode _ _ _ t a b c -> func3Source (valueTypeSource t) a b c
			Combine4VecNode _ _ _ _ t a b c d -> func4Source (valueTypeSource t) a b c d

		binaryOpSource :: Builder -> Node a -> Node b -> Builder
		binaryOpSource op a b = "(" <> nodeSource a <> ") " <> op <> " (" <> nodeSource b <> ")"

		func1Source :: Builder -> Node a -> Builder
		func1Source func a = func
			<> "("
			<> nodeSource a
			<> ")"

		func2Source :: Builder -> Node a -> Node b -> Builder
		func2Source func a b = func
			<> "("
			<> nodeSource a
			<> ", "
			<> nodeSource b
			<> ")"

		func3Source :: Builder -> Node a -> Node b -> Node c -> Builder
		func3Source func a b c = func
			<> "("
			<> nodeSource a
			<> ", "
			<> nodeSource b
			<> ", "
			<> nodeSource c
			<> ")"

		func4Source :: Builder -> Node a -> Node b -> Node c -> Node d -> Builder
		func4Source func a b c d = func
			<> "("
			<> nodeSource a
			<> ", "
			<> nodeSource b
			<> ", "
			<> nodeSource c
			<> ", "
			<> nodeSource d
			<> ")"
