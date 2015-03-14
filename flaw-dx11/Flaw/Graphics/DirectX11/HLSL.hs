{-|
Module: Flaw.Graphics.DirectX11.Internal
Description: Internals of graphics implementation for DirectX 11.
License: MIT
-}

{-# LANGUAGE GADTs, TypeFamilies #-}

module Flaw.Graphics.DirectX11.HLSL
	( HlslShader(..)
	, HlslAttribute(..)
	, HlslProgram(..)
	, generateProgram
	) where

import Data.Char
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

import Flaw.Graphics.Program.Internal
import qualified Flaw.Graphics.Program.SL as SL

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
		(_, attributes, _, _) = vsInfo
		as = map (\attribute@Attribute
			{ attributeSlot = slot
			, attributeOffset = offset
			} -> HlslAttribute
			{ hlslAttributeSemantic = TL.toStrict $ TLB.toLazyText $ attributeSemantic slot offset
			, hlslAttributeInfo = attribute
			}) attributes
		vs = hlslShader VertexStage vsInfo
		ps = hlslShader PixelStage psInfo
		in HlslVertexPixelProgram as vs ps

hlslShader :: Stage -> SL.ShaderInfo -> HlslShader
hlslShader stage (temps, attributes, uniforms, samplers) = HlslShader
	{ hlslShaderSource = TL.toStrict $ TLB.toLazyText source
	, hlslShaderEntryPoint = entryPoint
	, hlslShaderProfile = profile
	, hlslShaderUniforms = uniforms
	, hlslShaderSamplers = samplers
	} where
	(entryPoint, profile) = case stage of
		VertexStage -> (T.pack "VS", T.pack "vs_4_0")
		PixelStage -> (T.pack "PS", T.pack "ps_4_0")
		_ -> undefined
	source = attributesSource `mappend` uniformsSource `mappend` samplersSource `mappend` codeSource
	attributesSource = (foldl mappend (TLB.fromString "struct A\n{\n") $ map attributeSource attributes) `mappend` (TLB.fromString "};\n")
	attributeSource Attribute
		{ attributeSlot = slot
		, attributeOffset = offset
		, attributeValueType = vt
		}
		= (TLB.fromString "\t")
		`mappend` valueTypeSource vt
		`mappend` (TLB.singleton ' ')
		`mappend` attributeName slot offset
		`mappend` (TLB.fromString " : ")
		`mappend` attributeSemantic slot offset
		`mappend` (TLB.fromString ";\n")
	uniformsSource = mempty
	samplersSource = mempty
	codeSource = mempty

valueTypeSource :: ValueType -> TLB.Builder
valueTypeSource vt = case vt of
	ScalarValueType st -> scalarTypeSource st
	VectorValueType d st -> scalarTypeSource st `mappend` dimensionSource d
	MatrixValueType d1 d2 st
		-> scalarTypeSource st
		`mappend` dimensionSource d1
		`mappend` TLB.singleton 'x'
		`mappend` dimensionSource d2

scalarTypeSource :: ScalarType -> TLB.Builder
scalarTypeSource st = TLB.fromString $ case st of
	ScalarFloat -> "float"
	ScalarDouble -> "double"
	ScalarInt -> "int"
	ScalarUint -> "uint"
	ScalarBool -> "bool"

dimensionSource :: Dimension -> TLB.Builder
dimensionSource d = TLB.singleton $ case d of
	Dimension1 -> '1'
	Dimension2 -> '2'
	Dimension3 -> '3'
	Dimension4 -> '4'

attributeName :: Int -> Int -> TLB.Builder
attributeName slot offset
	= TLB.singleton 'v'
	`mappend` (TLB.fromString $ show slot)
	`mappend` (TLB.singleton '_')
	`mappend` (TLB.fromString $ show offset)

attributeSemantic :: Int -> Int -> TLB.Builder
attributeSemantic slot offset = semanticString slot `mappend` (TLB.fromString "_") `mappend` semanticString offset

-- | Calculate semantic string for a number.
semanticString :: Int -> TLB.Builder
semanticString = TLB.fromString . (str 8) where
	str :: Int -> Int -> String
	str 0 _ = ""
	str n i = (chr (ord 'A' + i `rem` 26)) : str (n - 1) (i `div` 26)
