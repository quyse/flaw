{-|
Module: Flaw.Graphics.DirectX11.HLSL
Description: HLSL generator for DirectX11 graphics.
License: MIT
-}

{-# LANGUAGE GADTs, OverloadedStrings, TypeFamilies #-}

module Flaw.Graphics.DirectX11.HLSL
	( HlslAttribute(..)
	, HlslShader(..)
	, HlslStage(..)
	, HlslProgram(..)
	, hlslGenerateProgram
	) where

import Data.Char
import Data.List
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder

import Flaw.Graphics.Program.Internal
import qualified Flaw.Graphics.Program.SL as SL
import Flaw.Math

-- | HLSL input or output.
data HlslVar = HlslVar
	{ hlslVarName :: Builder
	, hlslVarSemantic :: Builder
	, hlslVarType :: !ValueType
	}

-- | HLSL attribute.
data HlslAttribute = HlslAttribute
	{ hlslAttributeSemantic :: !T.Text
	, hlslAttributeInfo :: Attribute
	} deriving Show

-- | HLSL shader.
data HlslShader = HlslShader
	{ hlslShaderSource :: !T.Text
	, hlslShaderEntryPoint :: !T.Text
	, hlslShaderProfile :: !T.Text
	} deriving Show

data HlslStage
	= HlslVertexStage
	| HlslPixelStage
	deriving (Eq, Ord, Show)

-- | HLSL program.
data HlslProgram = HlslProgram
	{ hlslProgramAttributes :: [HlslAttribute]
	, hlslProgramShaders :: [(HlslStage, HlslShader)]
	} deriving Show

-- | Generate shader programs in HLSL.
hlslGenerateProgram :: State -> HlslProgram
hlslGenerateProgram state = program where
	-- generate program information
	SL.ProgramInfo shaders = SL.programInfo state

	-- generate shaders and unite them
	program = foldr mergeProgram emptyProgram shaders
	mergeProgram shader@(stage, _shaderInfo) HlslProgram
		{ hlslProgramAttributes = pas
		, hlslProgramShaders = ps
		} = HlslProgram
		{ hlslProgramAttributes = as ++ pas
		, hlslProgramShaders = (hlslStage, s) : ps
		} where
		(s, as) = generateShader shader
		hlslStage = case stage of
			VertexStage -> HlslVertexStage
			PixelStage -> HlslPixelStage
			_ -> error "wrong stage"
	emptyProgram = HlslProgram
		{ hlslProgramAttributes = []
		, hlslProgramShaders = []
		}

	-- function to generate shader and attributes
	generateShader :: (Stage, SL.ShaderInfo) -> (HlslShader, [HlslAttribute])
	generateShader (stage, SL.ShaderInfo
		{ SL.shaderTemps = temps
		, SL.shaderAttributes = attributes
		, SL.shaderUniforms = uniforms
		, SL.shaderSamplers = samplers
		, SL.shaderTargets = targets
		}) = (HlslShader
		{ hlslShaderSource = TL.toStrict $ toLazyText source
		, hlslShaderEntryPoint = TL.toStrict $ toLazyText entryPoint
		, hlslShaderProfile = TL.toStrict $ toLazyText profile
		}, resAttributes) where

		-- HLSL attributes
		resAttributes = map resAttribute attributes
		resAttribute attribute@Attribute
			{ attributeSlot = slot
			, attributeOffset = offset
			} = HlslAttribute
			{ hlslAttributeSemantic = TL.toStrict $ toLazyText $ attributeSemantic slot offset
			, hlslAttributeInfo = attribute
			}

		-- source
		source = inputsSource <> outputsSource <> uniformBuffersSource <> samplersSource <> codeSource

		-- inputs source
		inputsSource = "struct Input\n{\n" <> foldr (mappend . varSource) mempty inputs <> "};\n"
		inputs = map varAttribute attributes ++ map varInterpolant inInterpolants
		-- in-interpolants are temps used during this stage, but defined at another stage
		inInterpolants = filter (\temp -> tempStage temp /= stage) temps

		-- outputs source
		outputsSource = "struct Output\n{\n" <> foldr (mappend . varSource) mempty outputs <> "};\n"
		-- important: targets should be after interpolants, to have them in the same registers in accepting stage
		outputs = map varInterpolant outInterpolants ++ concatMap varsTarget targets
		-- out-interpolants are temps defined at this stage, but used by some other stage
		outInterpolants = filter (\temp -> tempStage temp == stage && tempUsedByOtherStage (tempIndex temp)) temps
		otherShaderInfos = concatMap (\(otherStage, otherShaderInfo) -> if otherStage == stage then [] else [otherShaderInfo]) shaders
		tempUsedByOtherStage i = any (elem i . map tempIndex . SL.shaderTemps) otherShaderInfos

		-- uniform buffers source
		uniformBuffers = groupBy eqUniformBySlot $ sortBy compareUniformBySlot uniforms
		uniformBuffersSource = foldr (mappend . uniformBufferSource) mempty uniformBuffers
		uniformBufferSource bufferUniforms = bufferHeader <> bufferSource <> bufferFooter where
			bufferHeader = "cbuffer CB" <> fromString (show slot) <> " : register(b" <> fromString (show slot) <> ")\n{\n"
			bufferFooter = "};\n"
			bufferSource = foldr (mappend . uniformInBufferSource) mempty bufferUniforms
			uniformInBufferSource Uniform
				{ uniformOffset = offset
				, uniformSize = size
				, uniformType = t
				} = "\t" <> valueTypeSource t <> " " <> uniformName slot offset
				<> (if size > 0 then "[" <> fromString (show size) <> "]" else mempty)
				<> " : packoffset(c" <> fromString (show $ offset `quot` 16)
				<> (if registerOffset > 0 then "." <> singleton ("xyzw" !! (registerOffset `quot` 4)) else mempty)
				<> ");\n"
				where registerOffset = offset `mod` 16
			slot = uniformSlot $ head bufferUniforms
		eqUniformBySlot a b = uniformSlot a == uniformSlot b
		compareUniformBySlot a b = compare (uniformSlot a) (uniformSlot b)

		-- samplers source
		samplersSource = foldr (mappend . samplerSource) mempty samplers
		samplerSource Sampler
			{ samplerSlot = slot
			, samplerDimension = dimension
			, samplerSampleType = sampleType
			} = dim <> "<" <> valueTypeSource sampleType <> "> t" <> fromString (show slot)
			<> " : register(t" <> fromString (show slot) <> ");\n"
			<> "SamplerState s" <> fromString (show slot) <> " : register(s" <> fromString (show slot) <> ");\n"
			where dim = case dimension of
				Sampler1D -> "Texture1D"
				Sampler2D -> "Texture2D"
				Sampler3D -> "Texture3D"
				SamplerCube -> "TextureCube"

		-- code source
		codeSource = "Output " <> entryPoint <> "(Input input, uint sI : SV_InstanceID" <> (if stage == PixelStage then ", float4 sP : SV_Position" else mempty) <> ")\n{\n\tOutput output;\n"
			<> tempsSource <> targetsSource <> outInterpolantsAssignmentsSource <> "\treturn output;\n}\n"

		-- definitions of temp variables
		tempsSource = foldr (mappend . tempSource) mempty temps
		tempSource Temp
			{ tempIndex = i
			, tempNode = node
			, tempStage = ts
			, tempType = t
			} = "\t" <> valueTypeSource t <> " " <> tempName i <> " = " <> (if ts == stage then nodeSource node else "input." <> interpolantName i) <> ";\n"

		-- assignments to out-interpolants
		outInterpolantsAssignmentsSource = foldr (mappend . outInterpolantAssignmentSource) mempty outInterpolants
		outInterpolantAssignmentSource Temp
			{ tempIndex = i
			} = "\toutput." <> interpolantName i <> " = " <> tempName i <> ";\n"

		-- outputting targets
		targetsSource = foldr (mappend . targetSource) mempty targets
		targetSource target = case target of
			PositionTarget node -> "\toutput.vTP = " <> nodeSource node <> ";\n"
			ColorTarget i node -> "\toutput." <> targetColorName i <> " = " <> nodeSource node <> ";\n"
			DualColorTarget nodeA nodeB -> targetSource (ColorTarget 0 nodeA) <> targetSource (ColorTarget 1 nodeB)
			DepthTarget node -> "\toutput." <> targetDepthName <> " = " <> nodeSource node <> ";\n"

		-- entry point name and HLSL profile
		(entryPoint, profile) = case stage of
			VertexStage -> ("VS", "vs_4_0")
			PixelStage -> ("PS", "ps_4_0")
			_ -> undefined

		-- helper functions

		-- convert attribute to var
		varAttribute Attribute
			{ attributeSlot = slot
			, attributeOffset = offset
			, attributeValueType = t
			} = HlslVar
			{ hlslVarName = attributeName slot offset
			, hlslVarSemantic = attributeSemantic slot offset
			, hlslVarType = t
			}

		-- convert interpolant to var
		varInterpolant Temp
			{ tempIndex = i
			, tempType = t
			} = HlslVar
			{ hlslVarName = interpolantName i
			, hlslVarSemantic = semanticString i
			, hlslVarType = t
			}

		-- convert target to var
		varsTarget target = case target of
			PositionTarget _ -> [HlslVar
				{ hlslVarName = "vTP"
				, hlslVarSemantic = "SV_Position"
				, hlslVarType = valueType (undefined :: Float4)
				}]
			ColorTarget i _ -> [HlslVar
				{ hlslVarName = targetColorName i
				, hlslVarSemantic = "SV_Target" <> fromString (show i)
				, hlslVarType = valueType (undefined :: Float4)
				}]
			DualColorTarget a b -> varsTarget (ColorTarget 0 a) ++ varsTarget (ColorTarget 1 b)
			DepthTarget _ -> [HlslVar
				{ hlslVarName = targetDepthName
				, hlslVarSemantic = "SV_Depth"
				, hlslVarType = valueType (undefined :: Float)
				}]

		-- calculate var source
		varSource HlslVar
			{ hlslVarName = name
			, hlslVarSemantic = semantic
			, hlslVarType = t
			} = "\t" <> valueTypeSource t <> " " <> name <> " : " <> semantic <> ";\n"

		attributeName :: Int -> Int -> Builder
		attributeName slot offset
			= "a"
			<> fromString (show slot)
			<> "_"
			<> fromString (show offset)

		attributeSemantic :: Int -> Int -> Builder
		attributeSemantic slot offset = semanticString slot <> "_" <> semanticString offset

		valueTypeSource :: ValueType -> Builder
		valueTypeSource vt = case vt of
			ScalarValueType st -> scalarTypeSource st
			VectorValueType d st -> scalarTypeSource st <> dimensionSource d
			MatrixValueType d1 d2 st
				-> scalarTypeSource st
				<> dimensionSource d1
				<> "x"
				<> dimensionSource d2

		scalarTypeSource :: ScalarType -> Builder
		scalarTypeSource st = case st of
			ScalarFloat -> "float"
			ScalarDouble -> "double"
			ScalarInt -> "int"
			ScalarUint -> "uint"
			ScalarBool -> "bool"

		dimensionSource :: Dimension -> Builder
		dimensionSource d = case d of
			Dimension1 -> "1"
			Dimension2 -> "2"
			Dimension3 -> "3"
			Dimension4 -> "4"

		-- | HLSL needs text semantics. Generate such a text for string.
		semanticString :: Int -> Builder
		semanticString = fromString . str 8 where
			str :: Int -> Int -> String
			str 0 _ = ""
			str n i = chr (ord 'A' + i `rem` 26) : str (n - 1) (i `quot` 26)

		interpolantName :: Int -> Builder
		interpolantName i = "i" <> fromString (show i)

		tempName :: Int -> Builder
		tempName i = "_" <> fromString (show i)

		targetColorName :: Int -> Builder
		targetColorName i = "tc" <> fromString (show i)

		targetDepthName :: Builder
		targetDepthName = "td"

		uniformName :: Int -> Int -> Builder
		uniformName slot offset = "u" <> fromString (show slot) <> "_" <> fromString (show offset)

		nodeSource :: Node a -> Builder
		nodeSource node = case node of
			AttributeNode Attribute
				{ attributeSlot = slot
				, attributeOffset = offset
				} -> "input." <> attributeName slot offset
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
			ClampNode _ a b c -> func3Source "clamp" a b c
			LerpNode _ a b c -> func3Source "lerp" a b c
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
			InvSqrtNode _ a -> func1Source "rsqrt" a
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
			MulNode _ _ _ a b -> func2Source "mul" a b
			DotNode _ _ a b -> func2Source "dot" a b
			CrossNode _ a b -> func2Source "cross" a b
			NormNode _ _ a -> func1Source "length" a
			Norm2Node _ _ a -> func1Source "length2" a
			NormalizeNode _ a -> func1Source "normalize" a
			DdxNode _ a -> func1Source "ddx" a
			DdyNode _ a -> func1Source "ddy" a
			InstanceIdNode -> "sI"
			ComponentNode _ _ c a -> "(" <> nodeSource a <> ")." <> singleton c
			SwizzleNode _ _ s a ->  "(" <> nodeSource a <> ")." <> fromString s
			SampleNode
				{ sampleNodeSamplerNode = SamplerNode Sampler
					{ samplerSlot = slot
					}
				, sampleNodeCoordsNode = c
				, sampleNodeOffsetNode = mo
				, sampleNodeLod = ll
				} -> let
				sc = "(s" <> fromString (show slot) <> ", " <> nodeSource c
				f = case ll of
					SampleNodeAutoLod -> "Sample" <> sc
					SampleNodeLod l -> "SampleLevel" <> sc <> ", " <> nodeSource l
					SampleNodeBiasLod b -> "SampleBias" <> sc <> ", " <> nodeSource b
					SampleNodeGradLod gx gy -> "SampleGrad" <> sc <> ", " <> nodeSource gx <> ", " <> nodeSource gy
				in "t" <> fromString (show slot) <> "." <> f <> case mo of
					Just o -> ", " <> nodeSource o <> ")"
					Nothing -> ")"
			CastNode _ t a -> "(" <> valueTypeSource t <> ")(" <> nodeSource a <> ")"
			Combine2VecNode _ _ t a b -> func2Source (valueTypeSource t) a b
			Combine3VecNode _ _ _ t a b c -> func3Source (valueTypeSource t) a b c
			Combine4VecNode _ _ _ _ t a b c d -> func4Source (valueTypeSource t) a b c d
			ScreenToTextureNode _ a -> "(" <> nodeSource a <> ") * float2(0.5f, -0.5f) + float2(0.5f, 0.5f)"
			FragCoordNode -> "sP"

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
