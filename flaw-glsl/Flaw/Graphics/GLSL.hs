{-|
Module: Flaw.Graphics.GLSL
Description: GLSL generator for WebGL graphics.
License: MIT
-}

{-# LANGUAGE GADTs, OverloadedStrings, TypeFamilies #-}

module Flaw.Graphics.GLSL
	( GlslAttribute(..)
	, GlslUniform(..)
	, GlslSampler(..)
	, GlslProgram(..)
	, GlslShader(..)
	, generateProgram
	) where

import Data.List
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder

import Flaw.Graphics.Program.Internal
import qualified Flaw.Graphics.Program.SL as SL

-- | GLSL input or output.
data GlslVar = GlslVar
	{ glslVarName :: Builder
	, glslVarType :: ValueType
	}

-- | GLSL shader.
newtype GlslShader = GlslShader T.Text
	deriving Show

data GlslAttribute = GlslAttribute
	{ glslAttributeName :: T.Text
	, glslAttributeInfo :: Attribute
	} deriving Show

data GlslUniform = GlslUniform
	{ glslUniformName :: T.Text
	, glslUniformInfo :: Uniform
	} deriving Show

data GlslSampler = GlslSampler
	{ glslSamplerName :: T.Text
	, glslSamplerInfo :: Sampler
	} deriving Show

-- | GLSL program.
data GlslProgram
	= GlslVertexPixelProgram [GlslAttribute] [GlslUniform] [GlslSampler] GlslShader GlslShader
	deriving Show

-- | Generate shader programs in GLSL.
generateProgram :: State -> GlslProgram
generateProgram state = case SL.programInfo state of
	SL.VertexPixelProgramInfo vsInfo psInfo -> let
		(_, attributes, vsUniforms, vsSamplers, _vsTargets) = vsInfo
		(psTemps, _, psUniforms, psSamplers, _psTargets) = psInfo
		as = map (\attribute@Attribute
			{ attributeSlot = slot
			, attributeOffset = offset
			} -> GlslAttribute
			{ glslAttributeName = TL.toStrict $ toLazyText $ attributeName slot offset
			, glslAttributeInfo = attribute
			}) attributes

		-- inputs for vertex shader - attributes
		attributeInputs = map attributeInput attributes

		-- interpolants
		interpolantTemps = filter (\temp -> tempStage temp == VertexStage) psTemps
		interpolants = map interpolant interpolantTemps
		interpolant Temp
			{ tempIndex = i
			, tempType = t
			} = GlslVar
			{ glslVarName = interpolantName i
			, glslVarType = t
			}

		-- program uniforms
		programUniform info@Uniform
			{ uniformSlot = slot
			, uniformOffset = offset
			} = GlslUniform
			{ glslUniformName = TL.toStrict $ toLazyText $ uniformName slot offset
			, glslUniformInfo = info
			}
		uniforms = map (programUniform . head) $ group $ sort $ vsUniforms ++ psUniforms

		-- program samplers
		programSampler info@Sampler
			{ samplerSlot = slot
			} = GlslSampler
			{ glslSamplerName = TL.toStrict $ toLazyText $ samplerName slot
			, glslSamplerInfo = info
			}
		samplers = map (programSampler . head) $ group $ sort $ vsSamplers ++ psSamplers

		vs = glslShader VertexStage vsInfo attributeInputs interpolants $ map tempIndex interpolantTemps
		ps = glslShader PixelStage psInfo [] interpolants []

		in GlslVertexPixelProgram as uniforms samplers vs ps

glslShader :: Stage -> SL.ShaderInfo -> [GlslVar] -> [GlslVar] -> [Int] -> GlslShader
glslShader stage (temps, _, uniforms, samplers, targets) attributes varyings interpolants = GlslShader $ TL.toStrict $ toLazyText source where
	source = headerSource <> attributesSource <> varyingsSource <> uniformsSource <> samplersSource <> codeSource
	headerSource = "#ifdef GL_ES\nprecision highp float;\n#endif\n"
	attributesSource = foldr mappend "" $ map attributeSource attributes
	varyingsSource = foldr mappend "" $ map varyingSource varyings
	attributeSource GlslVar
		{ glslVarName = name
		, glslVarType = t
		} = "attribute " <> valueTypeSource t <> " " <> name <> ";\n"
	varyingSource GlslVar
		{ glslVarName = name
		, glslVarType = t
		} = "varying " <> valueTypeSource t <> " " <> name <> ";\n"
	uniformsSource = foldr mappend mempty $ map uniformSource uniforms
	samplersSource = foldr mappend mempty $ map samplerSource samplers
	codeSource = "void main()\n{\n" <> tempsSource <> interpolantsSource <> targetsSource <> "}\n"
	tempsSource = foldr mappend mempty $ map tempSource temps
	tempSource Temp
		{ tempIndex = i
		, tempNode = node
		, tempStage = ts
		, tempType = t
		} = "\t" <> valueTypeSource t <> " " <> tempName i <> " = " <> (if ts == stage then nodeSource node else interpolantName i) <> ";\n"
	interpolantsSource = foldr mappend mempty $ map interpolantSource interpolants
	interpolantSource i = "\t" <> interpolantName i <> " = " <> tempName i <> ";\n"
	targetsSource = foldr mappend mempty $ map targetSource targets
	targetSource target = "\t" <> case target of
		PositionTarget node -> targetPositionName <> " = " <> nodeSource node <> ";\n"
		ColorTarget slot node -> targetColorName slot <> " = " <> nodeSource node <> ";\n"
		DepthTarget node -> targetDepthName <> " = " <> nodeSource node <> ";\n"

valueTypeSource :: ValueType -> Builder
valueTypeSource vt = case vt of
	ScalarValueType st -> scalarTypeSource st
	VectorValueType d st -> scalarShortTypeSource st <> "vec" <> dimensionSource d
	MatrixValueType d1 d2 st ->
		if d1 == d2 then
			scalarShortTypeSource st <> "mat" <> dimensionSource d1
		else
			scalarShortTypeSource st <> "mat" <> dimensionSource d1 <> "x" <> dimensionSource d2

scalarTypeSource :: ScalarType -> Builder
scalarTypeSource st = case st of
	ScalarFloat -> "float"
	ScalarDouble -> "double"
	ScalarInt -> "int"
	ScalarUint -> "uint"
	ScalarBool -> "bool"

scalarShortTypeSource :: ScalarType -> Builder
scalarShortTypeSource st = case st of
	ScalarFloat -> ""
	ScalarDouble -> "d"
	ScalarInt -> "i"
	ScalarUint -> "u"
	ScalarBool -> "b"

dimensionSource :: Dimension -> Builder
dimensionSource d = case d of
	Dimension1 -> "1"
	Dimension2 -> "2"
	Dimension3 -> "3"
	Dimension4 -> "4"

attributeInput :: Attribute -> GlslVar
attributeInput Attribute
	{ attributeSlot = slot
	, attributeOffset = offset
	, attributeValueType = t
	} = GlslVar
	{ glslVarName = attributeName slot offset
	, glslVarType = t
	}

attributeName :: Int -> Int -> Builder
attributeName slot offset
	= "a"
	<> fromString (show slot)
	<> "_"
	<> fromString (show offset)

interpolantName :: Int -> Builder
interpolantName i = "i" <> fromString (show i)

tempName :: Int -> Builder
tempName i = "_" <> fromString (show i)

targetPositionName :: Builder
targetPositionName = "gl_Position"

targetColorName :: Int -> Builder
targetColorName 0 = "gl_FragColor"
targetColorName i = "gl_FragData[" <> fromString (show i) <> "]"

targetDepthName :: Builder
targetDepthName = "gl_FragDepth"

uniformName :: Int -> Int -> Builder
uniformName slot offset = "u" <> fromString (show slot) <> "_" <> fromString (show offset)

uniformSource :: Uniform -> Builder
uniformSource Uniform
	{ uniformSlot = slot
	, uniformOffset = offset
	, uniformSize = size
	, uniformType = t
	} = source where
	source = "uniform "
		<> valueTypeSource t
		<> " "
		<> uniformName slot offset
		<> (if size > 0 then "[" <> fromString (show size) <> "]" else mempty)
		<> ";\n"

samplerName :: Int -> Builder
samplerName slot = "s" <> fromString (show slot)

samplerSource :: Sampler -> Builder
samplerSource Sampler
	{ samplerSlot = slot
	, samplerDimension = dimension
	, samplerSampleType = sampleType
	} = source where
	typeSource = case sampleType of
		ScalarValueType st -> scalarShortTypeSource st
		VectorValueType _dim st -> scalarShortTypeSource st
		MatrixValueType _dim1 _dim2 st -> scalarShortTypeSource st
	samplerDimensionSource = case dimension of
		Sampler1D -> "sampler1D"
		Sampler2D -> "sampler2D"
		Sampler3D -> "sampler3D"
		SamplerCube -> "samplerCube"
	source = "uniform " <> typeSource <> samplerDimensionSource <> " s" <> fromString (show slot) <> ";\n"

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
			VectorValueType _ _ -> concat $ intersperse ", " s
			MatrixValueType _ _ _ -> concat $ intersperse ", " s
		in valueTypeSource t <> "(" <> fromString content <> ")"
	IndexNode _ _ a b -> "(" <> nodeSource a <> ")[" <> nodeSource b <> "]"
	AddNode _ a b -> binaryOpSource '+' a b
	SubtractNode _ a b -> binaryOpSource '-' a b
	MultiplyNode _ a b -> binaryOpSource '*' a b
	DivideNode _ a b -> binaryOpSource '/' a b
	RecipNode _ a -> func1Source "rcp" a
	NegateNode _ a -> "-(" <> nodeSource a <> ")"
	AbsNode _ a -> func1Source "abs" a
	SignumNode _ a -> func1Source "sign" a
	MinNode _ a b -> func2Source "min" a b
	MaxNode _ a b -> func2Source "max" a b
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
	MulNode _ _ _ a b -> binaryOpSource '*' a b
	DotNode _ _ a b -> func2Source "dot" a b
	NormNode _ _ a -> func1Source "length" a
	Norm2Node _ _ a -> func1Source "length2" a
	NormalizeNode _ a -> func1Source "normalize" a
	DdxNode _ a -> func1Source "ddx" a
	DdyNode _ a -> func1Source "ddy" a
	InstanceIdNode -> "sI"
	ComponentNode _ _ c a -> "(" <> nodeSource a <> ")." <> singleton c
	SwizzleNode _ _ s a ->  "(" <> nodeSource a <> ")." <> fromString s
	SampleNode (SamplerNode Sampler
		{ samplerSlot = slot
		, samplerSampleType = sampleType
		}) c -> "texture2D(s" <> fromString (show slot) <> ", " <> nodeSource c <> ")"
		<> case sampleType of
			ScalarValueType _ -> ".x"
			VectorValueType dim _ -> case dim of
				Dimension1 -> ".x"
				Dimension2 -> ".xy"
				Dimension3 -> ".xyz"
				Dimension4 -> mempty
			MatrixValueType _ _ _ -> mempty
	CastNode _ t a -> valueTypeSource t <> "(" <> nodeSource a <> ")"
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
