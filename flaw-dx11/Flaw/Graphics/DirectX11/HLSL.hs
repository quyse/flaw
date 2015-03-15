{-|
Module: Flaw.Graphics.DirectX11.Internal
Description: Internals of graphics implementation for DirectX 11.
License: MIT
-}

{-# LANGUAGE GADTs, OverloadedStrings, TypeFamilies #-}

module Flaw.Graphics.DirectX11.HLSL
	( HlslAttribute(..)
	, HlslProgram(..)
	, HlslShader(..)
	, generateProgram
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
	, hlslVarType :: ValueType
	}

-- | HLSL shader.
data HlslShader = HlslShader
	{ hlslShaderSource :: T.Text
	, hlslShaderEntryPoint :: T.Text
	, hlslShaderProfile :: T.Text
	, hlslShaderUniforms :: [Uniform]
	, hlslShaderSamplers :: [Sampler]
	} deriving Show

data HlslAttribute = HlslAttribute
	{ hlslAttributeSemantic :: T.Text
	, hlslAttributeInfo :: Attribute
	} deriving Show

-- | HLSL program.
data HlslProgram
	= HlslVertexPixelProgram [HlslAttribute] HlslShader HlslShader
	deriving Show

-- | Generate shader programs in HLSL.
generateProgram :: State -> HlslProgram
generateProgram state = case SL.programInfo state of
	SL.VertexPixelProgramInfo vsInfo psInfo -> let
		(_, attributes, _, _, vsTargets) = vsInfo
		(psTemps, _, _, _, psTargets) = psInfo
		as = map (\attribute@Attribute
			{ attributeSlot = slot
			, attributeOffset = offset
			} -> HlslAttribute
			{ hlslAttributeSemantic = TL.toStrict $ toLazyText $ attributeSemantic slot offset
			, hlslAttributeInfo = attribute
			}) attributes

		-- inputs for vertex shader - attributes
		vsInputs = map attributeInput attributes

		-- interpolants
		interpolantTemps = filter (\temp -> tempStage temp == VertexStage) psTemps
		interpolants = map interpolant interpolantTemps
		interpolant Temp
			{ tempIndex = i
			, tempType = t
			} = HlslVar
			{ hlslVarName = interpolantName i
			, hlslVarSemantic = semanticString i
			, hlslVarType = t
			}

		-- outputs for vertex shader - position and interpolants
		vsOutputs = map targetOutput vsTargets ++ interpolants

		-- inputs for pixel shader - interpolants
		psInputs = interpolants

		-- ouputs for pixel shader - targets
		psOutputs = map targetOutput psTargets

		vs = hlslShader VertexStage vsInfo vsInputs vsOutputs $ map tempIndex interpolantTemps
		ps = hlslShader PixelStage psInfo psInputs psOutputs []

		in HlslVertexPixelProgram as vs ps

hlslShader :: Stage -> SL.ShaderInfo -> [HlslVar] -> [HlslVar] -> [Int] -> HlslShader
hlslShader stage (temps, _, uniforms, samplers, targets) inputs outputs interpolants = HlslShader
	{ hlslShaderSource = TL.toStrict $ toLazyText source
	, hlslShaderEntryPoint = TL.toStrict $ toLazyText entryPoint
	, hlslShaderProfile = profile
	, hlslShaderUniforms = uniforms
	, hlslShaderSamplers = samplers
	} where
	(entryPoint, profile) = case stage of
		VertexStage -> ("VS", "vs_4_0")
		PixelStage -> ("PS", "ps_4_0")
		_ -> undefined
	source = inputsSource <> outputsSource <> uniformsSource <> samplersSource <> codeSource
	inputsSource = "struct Input\n{\n" <> (foldr mappend "};\n" $ map varSource inputs)
	outputsSource = "struct Output\n{\n" <> (foldr mappend "};\n" $ map varSource outputs)
	varSource HlslVar
		{ hlslVarName = name
		, hlslVarSemantic = semantic
		, hlslVarType = t
		} = "\t" <> valueTypeSource t <> " " <> name <> " : " <> semantic <> ";\n"
	uniformsSource = foldr mappend mempty $ map uniformBufferSource $ groupBy (\a b -> uniformSlot a == uniformSlot b) uniforms
	samplersSource = foldr mappend mempty $ map samplerSource samplers
	codeSource = "Output " <> entryPoint <> "(Input input)\n{\n\tOutput output;\n" <> tempsSource <> targetsSource <> interpolantsSource <> "\treturn output;\n}\n"
	tempsSource = foldr mappend mempty $ map tempSource temps
	tempSource Temp
		{ tempIndex = i
		, tempNode = node
		, tempStage = ts
		, tempType = t
		} = "\t" <> valueTypeSource t <> " " <> tempName i <> " = " <> (if ts == stage then nodeSource node else "input." <> interpolantName i) <> ";\n"
	targetsSource = foldr mappend mempty $ map targetSource targets
	targetSource target = case target of
		PositionTarget node -> "\toutput.vTP = " <> nodeSource node <> ";\n"
		ColorTarget i node -> "\toutput." <> targetColorName i <> " = " <> nodeSource node <> ";\n"
		DepthTarget node -> "\toutput." <> targetDepthName <> " = " <> nodeSource node <> ";\n"
	interpolantsSource = foldr mappend mempty $ map interpolantSource interpolants
	interpolantSource i = "\toutput." <> interpolantName i <> " = " <> tempName i <> ";\n"

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

attributeInput :: Attribute -> HlslVar
attributeInput Attribute
	{ attributeSlot = slot
	, attributeOffset = offset
	, attributeValueType = t
	} = HlslVar
	{ hlslVarName = attributeName slot offset
	, hlslVarSemantic = attributeSemantic slot offset
	, hlslVarType = t
	}

attributeName :: Int -> Int -> Builder
attributeName slot offset
	= "a"
	<> fromString (show slot)
	<> "_"
	<> fromString (show offset)

attributeSemantic :: Int -> Int -> Builder
attributeSemantic slot offset = semanticString slot <> "_" <> semanticString offset

-- | Calculate semantic string for a number.
semanticString :: Int -> Builder
semanticString = fromString . (str 8) where
	str :: Int -> Int -> String
	str 0 _ = ""
	str n i = (chr (ord 'A' + i `rem` 26)) : str (n - 1) (i `div` 26)

interpolantName :: Int -> Builder
interpolantName i = "i" <> fromString (show i)

tempName :: Int -> Builder
tempName i = "_" <> fromString (show i)

targetColorName :: Int -> Builder
targetColorName i = "tc" <> fromString (show i)

targetDepthName :: Builder
targetDepthName = "td"

targetOutput :: Target -> HlslVar
targetOutput target = case target of
	PositionTarget _ -> HlslVar
		{ hlslVarName = "vTP"
		, hlslVarSemantic = "SV_Position"
		, hlslVarType = valueType (undefined :: Vec4f)
		}
	ColorTarget i _ -> HlslVar
		{ hlslVarName = targetColorName i
		, hlslVarSemantic = "SV_Target" <> fromString (show i)
		, hlslVarType = valueType (undefined :: Vec4f)
		}
	DepthTarget _ -> HlslVar
		{ hlslVarName = targetDepthName
		, hlslVarSemantic = "SV_Depth"
		, hlslVarType = valueType (undefined :: Float)
		}

uniformName :: Int -> Int -> Builder
uniformName slot offset = "u" <> fromString (show slot) <> "_" <> fromString (show offset)

uniformBufferSource :: [Uniform] -> Builder
uniformBufferSource [] = mempty
uniformBufferSource uniforms@(Uniform
	{ uniformSlot = slot
	} : _) = header <> (foldr mappend footer $ map def uniforms) where
	header = "cbuffer CB" <> fromString (show slot) <> " : register(b" <> fromString (show slot) <> ")\n{\n"
	footer = "};\n"
	def Uniform
		{ uniformOffset = offset
		, uniformSize = size
		, uniformType = t
		}
		= source where
		source = "\t"
			<> valueTypeSource t
			<> " "
			<> uniformName slot offset
			<> (if size > 0 then "[" <> fromString (show size) <> "]" else mempty)
			<> " : packoffset(c" <> fromString (show $ offset `div` 16)
			<> (if registerOffset > 0 then "." <> singleton ("xyzw" !! (registerOffset `div` 4)) else mempty)
			<> ");\n"
		registerOffset = offset `mod` 16

samplerSource :: Sampler -> Builder
samplerSource Sampler
	{ samplerSlot = slot
	, samplerDimension = dimension
	, samplerSampleType = sampleType
	} = (case dimension of
		Sampler1D -> "Texture1D"
		Sampler2D -> "Texture2D"
		Sampler3D -> "Texture3D"
		SamplerCube -> "TextureCube"
	)
	<> "<"
	<> valueTypeSource sampleType
	<> "> t" <> fromString (show slot) <> " : register(t" <> fromString (show slot) <> ");\n"
	<> "SamplerState s" <> fromString (show slot) <> " : register(s" <> fromString (show slot) <> ");\n"

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
			VectorValueType _ _ -> concat $ intersperse ", " s
			MatrixValueType _ _ _ -> concat $ intersperse ", " s
		in valueTypeSource t <> "(" <> fromString content <> ")"
	AddNode _ a b -> binaryOpSource '+' a b
	SubtractNode _ a b -> binaryOpSource '-' a b
	MultiplyNode _ a b -> binaryOpSource '*' a b
	DivideNode _ a b -> binaryOpSource '/' a b
	RecipNode _ a -> func1Source "rcp" a
	NegateNode _ a -> "-(" <> nodeSource a <> ")"
	AbsNode _ a -> func1Source "abs" a
	SignumNode _ a -> func1Source "sign" a
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
	MulNode _ _ _ a b -> func2Source "mul" a b
	DotNode _ _ a b -> func2Source "dot" a b
	InstanceIdNode -> "sI"
	ComponentNode _ _ c a -> "(" <> nodeSource a <> ")." <> singleton c
	SwizzleNode _ _ s a ->  "(" <> nodeSource a <> ")." <> fromString s
	SampleNode (SamplerNode Sampler
		{ samplerSlot = slot
		}) c -> "t" <> fromString (show slot) <> ".Sample(s" <> fromString (show slot) <> ", " <> nodeSource c <> ")"
	CastNode _ t a -> "(" <> valueTypeSource t <> ")(" <> nodeSource a <> ")"
	Combine2VecNode _ _ t a b -> func2Source (valueTypeSource t) a b
	Combine3VecNode _ _ _ t a b c -> func3Source (valueTypeSource t) a b c
	Combine4VecNode _ _ _ _ t a b c d -> func4Source (valueTypeSource t) a b c d

binaryOpSource :: Char -> Node a -> Node b -> Builder
binaryOpSource op a b = "(" <> nodeSource a <> ") " <> singleton op <> " (" <> nodeSource b <> ")"

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
