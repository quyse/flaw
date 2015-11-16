{-|
Module: Flaw.Graphics.Program.Internal
Description: Internals for shader program support.
License: MIT
-}

{-# LANGUAGE GADTs, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeFamilies, UndecidableInstances #-}

module Flaw.Graphics.Program.Internal
	( ScalarType(..)
	, Dimension(..)
	, ValueType(..)
	, valueTypeScalarsCount
	, OfScalarType(..)
	, OfVectorType
	, OfValueType(..)
	, OfAttributeType(..)
	, AttributeFormat(..)
	, AttributeType(..)
	, Normalization(..)
	, State(..)
	, Attribute(..)
	, Uniform(..)
	, Sampler(..)
	, SamplerDimension(..)
	, Target(..)
	, Stage(..)
	, Temp(..)
	, Node(..)
	, SamplerNode(..)
	, nodeValueType
	, nodeArrayValueType
	, Program
	, runProgram
	) where

import Control.Monad
import Control.Monad.Reader
import Data.Char
import Data.IORef
import Language.Haskell.TH

import Flaw.Math

-- | Supported scalar types in programs.
data ScalarType
	= ScalarFloat
	| ScalarDouble
	| ScalarInt
	| ScalarUint
	| ScalarBool
	deriving (Eq, Ord, Show)

-- | Supported dimensions in programs.
data Dimension
	= Dimension1
	| Dimension2
	| Dimension3
	| Dimension4
	deriving (Eq, Ord, Show)

-- | Supported types in programs.
data ValueType
	= ScalarValueType !ScalarType
	| VectorValueType !Dimension !ScalarType
	| MatrixValueType !Dimension !Dimension !ScalarType
	deriving (Eq, Ord, Show)

-- | Number of scalars in type.
valueTypeScalarsCount :: ValueType -> Int
valueTypeScalarsCount vt = case vt of
	ScalarValueType _ -> 1
	VectorValueType d _ -> dim d
	MatrixValueType d1 d2 _ -> dim d1 * dim d2
	where dim d = case d of
		Dimension1 -> 1
		Dimension2 -> 2
		Dimension3 -> 3
		Dimension4 -> 4

-- | Class of scalar types which can be used in program.
class OfValueType a => OfScalarType a where
	-- | Get program scalar type.
	-- Argument is not used.
	scalarType :: a -> ScalarType

instance OfScalarType Float where
	scalarType _ = ScalarFloat
instance OfScalarType Double where
	scalarType _ = ScalarDouble
instance OfScalarType Int where
	scalarType _ = ScalarInt
instance OfScalarType Word where
	scalarType _ = ScalarUint
instance OfScalarType Bool where
	scalarType _ = ScalarBool

-- | Class of types which can be used in program.
class Show a => OfValueType a where
	valueType :: a -> ValueType
	valueToShowList :: a -> [String]
	valueToShowList v = [show v]

instance OfValueType Float where
	valueType _ = ScalarValueType ScalarFloat
instance OfValueType Double where
	valueType _ = ScalarValueType ScalarDouble
instance OfValueType Int where
	valueType _ = ScalarValueType ScalarInt
instance OfValueType Word where
	valueType _ = ScalarValueType ScalarUint
instance OfValueType Bool where
	valueType _ = ScalarValueType ScalarBool

-- instance OfScalarType a => OfValueType (Vec{1..4} a)
liftM concat $ forM ['1'..'4'] $ \c -> do
	let name = mkName $ "Vec" ++ [c]
	let t = conT name
	let d = conE $ mkName $ "Dimension" ++ [c]
	ps <- forM ['1'..c] $ \p -> newName ['p', p]
	[d|
		instance OfScalarType a => OfValueType ($t a) where
			valueType _ = VectorValueType $d $ scalarType (undefined :: a)
			valueToShowList $(conP name $ map varP ps) = $(listE $ map (\p -> appE (varE 'show) $ varE p) ps)
		|]

-- instance OfScalarType a => OfValueType (Mat{1..4}x{1..4} a)
liftM concat $ forM [(i, j) | i <- ['1'..'4'], j <- ['1'..'4']] $ \(ci, cj) -> do
	let name = mkName $ "Mat" ++ [ci, 'x', cj]
	let t = conT name
	let di = conE $ mkName $ "Dimension" ++ [ci]
	let dj = conE $ mkName $ "Dimension" ++ [cj]
	ps <- forM [(i, j) | i <- ['1'..ci], j <- ['1'..cj]] $ \(i, j) -> newName ['p', i, j]
	[d|
		instance OfScalarType a => OfValueType ($t a) where
			valueType _ = MatrixValueType $di $dj $ scalarType (undefined :: a)
			valueToShowList $(conP name $ map varP ps) = $(listE $ map (\p -> appE (varE 'show) $ varE p) ps)
		|]

-- | Class of vector types which can be used in program.
class (OfValueType a, Vec a, OfScalarType (VecElement a)) => OfVectorType a

instance OfScalarType a => OfVectorType (Vec1 a)
instance OfScalarType a => OfVectorType (Vec2 a)
instance OfScalarType a => OfVectorType (Vec3 a)
instance OfScalarType a => OfVectorType (Vec4 a)

-- | Class of types which can be used in vertex attribute.
class OfValueType a => OfAttributeType a where
	-- | Typed attibute format.
	data AttributeFormat a :: *
	attributeFormatToType :: AttributeFormat a -> AttributeType

-- | Attribute format ids.
data AttributeType
	= ATFloat32
	| ATFloat16
	| ATInt32 !Normalization
	| ATInt16 !Normalization
	| ATInt8 !Normalization
	| ATUint32 !Normalization
	| ATUint16 !Normalization
	| ATUint8 !Normalization
	| ATVec1 !AttributeType
	| ATVec2 !AttributeType
	| ATVec3 !AttributeType
	| ATVec4 !AttributeType
	| ATMat1x1 !AttributeType
	| ATMat1x2 !AttributeType
	| ATMat1x3 !AttributeType
	| ATMat1x4 !AttributeType
	| ATMat2x1 !AttributeType
	| ATMat2x2 !AttributeType
	| ATMat2x3 !AttributeType
	| ATMat2x4 !AttributeType
	| ATMat3x1 !AttributeType
	| ATMat3x2 !AttributeType
	| ATMat3x3 !AttributeType
	| ATMat3x4 !AttributeType
	| ATMat4x1 !AttributeType
	| ATMat4x2 !AttributeType
	| ATMat4x3 !AttributeType
	| ATMat4x4 !AttributeType
	deriving (Eq, Ord, Show)

-- | Normalization mode.
data Normalization
	= NonNormalized
	| Normalized
	deriving (Eq, Ord, Show)

instance OfAttributeType Float where
	data AttributeFormat Float
		= AttributeFloat32
		| AttributeFloat16
	attributeFormatToType f = case f of
		AttributeFloat32 -> ATFloat32
		AttributeFloat16 -> ATFloat16

instance OfAttributeType Int where
	data AttributeFormat Int
		= AttributeInt32 !Normalization
		| AttributeInt16 !Normalization
		| AttributeInt8 !Normalization
	attributeFormatToType f = case f of
		AttributeInt32 n -> ATInt32 n
		AttributeInt16 n -> ATInt16 n
		AttributeInt8 n -> ATInt8 n

instance OfAttributeType Word where
	data AttributeFormat Word
		= AttributeUint32 !Normalization
		| AttributeUint16 !Normalization
		| AttributeUint8 !Normalization
	attributeFormatToType f = case f of
		AttributeUint32 n -> ATUint32 n
		AttributeUint16 n -> ATUint16 n
		AttributeUint8 n -> ATUint8 n

-- instance (OfScalarType a, OfAttributeType a) => OfAttributeType (Vec{1..4} a)
forM ['1'..'4'] $ \c -> do
	let v = mkName $ "Vec" ++ [c]
	a <- newName "a"
	let conName = mkName $ "AttributeVec" ++ [c]
	b <- newName "b"
	instanceD (sequence [ [t| OfScalarType $(varT a) |], [t| OfAttributeType $(varT a) |] ]) (appT (conT ''OfAttributeType) $ appT (conT v) $ varT a)
		[ dataInstD (return []) ''AttributeFormat [appT (conT v) $ varT a]
			[ normalC conName [return (NotStrict, AppT (ConT ''AttributeFormat) $ VarT a)]
			] []
		, funD 'attributeFormatToType [clause [conP conName [varP b]] (normalB [| $(conE $ mkName $ "ATVec" ++ [c]) (attributeFormatToType $(varE b)) |]) []]
		]

-- instance (OfScalarType a, OfAttributeType a) => OfAttributeType (Mat{1..4}x{1..4} a)
forM [(ci, cj) | ci <- ['1'..'4'], cj <- ['1'..'4']] $ \(ci, cj) -> do
	let v = mkName $ "Mat" ++ [ci, 'x', cj]
	a <- newName "a"
	let conName = mkName $ "AttributeMat" ++ [ci, 'x', cj]
	b <- newName "b"
	instanceD (sequence [ [t| OfScalarType $(varT a) |], [t| OfAttributeType $(varT a) |] ]) (appT (conT ''OfAttributeType) $ appT (conT v) $ varT a)
		[ dataInstD (return []) ''AttributeFormat [appT (conT v) $ varT a]
			[ normalC conName [return (NotStrict, AppT (ConT ''AttributeFormat) $ VarT a)]
			] []
		, funD 'attributeFormatToType [clause [conP conName [varP b]] (normalB [| $(conE $ mkName $ "ATMat" ++ [ci, 'x', cj]) (attributeFormatToType $(varE b)) |]) []]
		]

-- | State of the program while constructing.
data State = State
	{ stateStage :: Stage
	, stateTemps :: [Temp]
	, stateTempsCount :: !Int
	, stateTargets :: [Target]
	} deriving Show

data Attribute = Attribute
	{ attributeSlot :: !Int
	, attributeOffset :: !Int
	, attributeDivisor :: !Int
	, attributeType :: !AttributeType
	, attributeValueType :: !ValueType
	} deriving (Eq, Ord, Show)

data Uniform = Uniform
	{ uniformSlot :: !Int
	, uniformOffset :: !Int
	, uniformSize :: !Int
	, uniformType :: !ValueType
	} deriving (Eq, Ord, Show)

data Sampler = Sampler
	{ samplerSlot :: !Int
	, samplerDimension :: !SamplerDimension
	, samplerSampleType :: !ValueType
	, samplerCoordsType :: !ValueType
	} deriving (Eq, Ord, Show)

data SamplerDimension
	= Sampler1D
	| Sampler2D
	| Sampler3D
	| SamplerCube
	deriving (Eq, Ord, Show)

data Target
	= PositionTarget (Node Vec4f)
	| ColorTarget !Int (Node Vec4f)
	| DepthTarget (Node Float)
	deriving Show

data Stage
	= VertexStage
	| PixelStage
	| EndStage
	deriving (Eq, Ord, Show)

data Temp = forall a. OfValueType a => Temp
	{ tempIndex :: !Int
	, tempNode :: Node a
	, tempStage :: !Stage
	, tempType :: !ValueType
	}
deriving instance Show Temp

data Node a where
	AttributeNode :: Attribute -> Node a
	UniformNode :: Uniform -> Node a
	TempNode :: Int -> Node a
	ConstNode :: OfValueType a => ValueType -> a -> Node a
	IndexNode :: (OfValueType a, OfValueType b, Integral b) => ValueType -> ValueType -> Node [a] -> Node b -> Node a
	AddNode :: (OfValueType a, Num a) => ValueType -> Node a -> Node a -> Node a
	SubtractNode :: (OfValueType a, Num a) => ValueType -> Node a -> Node a -> Node a
	MultiplyNode :: (OfValueType a, Num a) => ValueType -> Node a -> Node a -> Node a
	DivideNode :: (OfValueType a, Fractional a) => ValueType -> Node a -> Node a -> Node a
	RecipNode :: (OfValueType a, Fractional a) => ValueType -> Node a -> Node a
	NegateNode :: (OfValueType a, Num a) => ValueType -> Node a -> Node a
	AbsNode :: (OfValueType a, Num a) => ValueType -> Node a -> Node a
	SignumNode :: (OfValueType a, Num a) => ValueType -> Node a -> Node a
	MinNode :: OfValueType a => ValueType -> Node a -> Node a -> Node a
	MaxNode :: OfValueType a => ValueType -> Node a -> Node a -> Node a
	PiNode :: (OfValueType a, Floating a) => ValueType -> Node a
	ExpNode :: (OfValueType a, Floating a) => ValueType -> Node a -> Node a
	SqrtNode :: (OfValueType a, Floating a) => ValueType -> Node a -> Node a
	LogNode :: (OfValueType a, Floating a) => ValueType -> Node a -> Node a
	PowNode :: (OfValueType a, Floating a) => ValueType -> Node a -> Node a -> Node a
	LogBaseNode :: (OfValueType a, Floating a) => ValueType -> Node a -> Node a -> Node a
	SinNode :: (OfValueType a, Floating a) => ValueType -> Node a -> Node a
	TanNode :: (OfValueType a, Floating a) => ValueType -> Node a -> Node a
	CosNode :: (OfValueType a, Floating a) => ValueType -> Node a -> Node a
	AsinNode :: (OfValueType a, Floating a) => ValueType -> Node a -> Node a
	AtanNode :: (OfValueType a, Floating a) => ValueType -> Node a -> Node a
	AcosNode :: (OfValueType a, Floating a) => ValueType -> Node a -> Node a
	SinhNode :: (OfValueType a, Floating a) => ValueType -> Node a -> Node a
	TanhNode :: (OfValueType a, Floating a) => ValueType -> Node a -> Node a
	CoshNode :: (OfValueType a, Floating a) => ValueType -> Node a -> Node a
	AsinhNode :: (OfValueType a, Floating a) => ValueType -> Node a -> Node a
	AtanhNode :: (OfValueType a, Floating a) => ValueType -> Node a -> Node a
	AcoshNode :: (OfValueType a, Floating a) => ValueType -> Node a -> Node a
	MulNode :: (OfValueType a, OfValueType b, Mul a b, OfValueType (MulResult a b)) => ValueType -> ValueType -> ValueType -> Node a -> Node b -> Node (MulResult a b)
	DotNode :: (OfVectorType v, OfScalarType (VecElement v), Dot v) => ValueType -> ValueType -> Node v -> Node v -> Node (VecElement v)
	NormNode :: (OfVectorType v, OfScalarType (VecElement v), Norm v) => ValueType -> ValueType -> Node v -> Node (VecElement v)
	Norm2Node :: (OfVectorType v, OfScalarType (VecElement v), Norm v) => ValueType -> ValueType -> Node v -> Node (VecElement v)
	NormalizeNode :: (OfVectorType v, Normalize v) => ValueType -> Node v -> Node v
	DdxNode :: OfValueType a => ValueType -> Node a -> Node a
	DdyNode :: OfValueType a => ValueType -> Node a -> Node a
	InstanceIdNode :: Node Word
	ComponentNode :: OfVectorType v => ValueType -> ValueType -> Char -> Node v -> Node (VecElement v)
	SwizzleNode :: (OfVectorType a, OfVectorType b) => ValueType -> ValueType -> String -> Node a -> Node b
	SampleNode :: SamplerNode s c -> Node c -> Node s
	CastNode :: (OfValueType a, OfValueType b) => ValueType -> ValueType -> Node a -> Node b
	Combine2VecNode
		:: (OfValueType a, OfValueType b, Combine2Vec a b, OfValueType (Combine2VecResult a b))
		=> ValueType -> ValueType -> ValueType -> Node a -> Node b -> Node (Combine2VecResult a b)
	Combine3VecNode
		:: (OfValueType a, OfValueType b, OfValueType c, Combine3Vec a b c, OfValueType (Combine3VecResult a b c))
		=> ValueType -> ValueType -> ValueType -> ValueType -> Node a -> Node b -> Node c -> Node (Combine3VecResult a b c)
	Combine4VecNode
		:: (OfValueType a, OfValueType b, OfValueType c, OfValueType d, Combine4Vec a b c d, OfValueType (Combine4VecResult a b c d))
		=> ValueType -> ValueType -> ValueType -> ValueType -> ValueType -> Node a -> Node b -> Node c -> Node d -> Node (Combine4VecResult a b c d)

deriving instance Show (Node a)

newtype SamplerNode s c = SamplerNode Sampler deriving Show

nodeValueType :: OfValueType a => Node a -> ValueType
nodeValueType node = valueType $ (undefined :: (Node a -> a)) node

nodeArrayValueType :: OfValueType a => Node [a] -> ValueType
nodeArrayValueType node = valueType $ (undefined :: (Node [a] -> a)) node

instance OfVectorType v => Vec (Node v) where
	type VecElement (Node v) = Node (VecElement v)
	vecLength _ = vecLength (undefined :: v)
	vecToList _ = undefined
	vecFromScalar e = CastNode (nodeValueType e) (valueType (undefined :: v)) e

instance (OfValueType a, Num a) => Num (Node a) where
	(+) = AddNode $ valueType (undefined :: a)
	(*) = MultiplyNode $ valueType (undefined :: a)
	(-) = SubtractNode $ valueType (undefined :: a)
	negate = NegateNode $ valueType (undefined :: a)
	abs = AbsNode $ valueType (undefined :: a)
	signum = SignumNode $ valueType (undefined :: a)
	fromInteger = (ConstNode $ valueType (undefined :: a)) . fromInteger

instance (OfValueType a, Fractional a) => Fractional (Node a) where
	(/) = DivideNode $ valueType (undefined :: a)
	recip = RecipNode $ valueType (undefined :: a)
	fromRational = (ConstNode $ valueType (undefined :: a)) . fromRational

instance (OfValueType a, Floating a) => Floating (Node a) where
	pi = PiNode $ valueType (undefined :: a)
	exp = ExpNode $ valueType (undefined :: a)
	sqrt = SqrtNode $ valueType (undefined :: a)
	log = LogNode $ valueType (undefined :: a)
	(**) = PowNode $ valueType (undefined :: a)
	logBase = LogBaseNode $ valueType (undefined :: a)
	sin = SinNode $ valueType (undefined :: a)
	tan = TanNode $ valueType (undefined :: a)
	cos = CosNode $ valueType (undefined :: a)
	asin = AsinNode $ valueType (undefined :: a)
	atan = AtanNode $ valueType (undefined :: a)
	acos = AcosNode $ valueType (undefined :: a)
	sinh = SinhNode $ valueType (undefined :: a)
	tanh = TanhNode $ valueType (undefined :: a)
	cosh = CoshNode $ valueType (undefined :: a)
	asinh = AsinhNode $ valueType (undefined :: a)
	atanh = AtanhNode $ valueType (undefined :: a)
	acosh = AcoshNode $ valueType (undefined :: a)

instance (OfValueType a, OfValueType b, OfValueType (MulResult a b), Mul a b) => Mul (Node a) (Node b) where
	type MulResult (Node a) (Node b) = Node (MulResult a b)
	mul = MulNode (valueType (undefined :: a)) (valueType (undefined :: b)) (valueType (undefined :: MulResult a b))

instance (OfVectorType v, Dot v) => Dot (Node v) where
	dot = DotNode (valueType (undefined :: v)) (valueType (undefined :: VecElement v))

instance (OfVectorType v, Norm v) => Norm (Node v) where
	norm = NormNode (valueType (undefined :: v)) (valueType (undefined :: VecElement v))
	norm2 = Norm2Node (valueType (undefined :: v)) (valueType (undefined :: VecElement v))

instance (OfVectorType v, Normalize v) => Normalize (Node v) where
	normalize = NormalizeNode (valueType (undefined :: v))

{- instance
	( OfVectorType v
	, OfScalarType (VecElement v)
	, Vec{X..W} v
	) => Vec{X..W} (Node v)
-}
forM "xyzw" $ \c -> do
	v <- newName "v"
	let vc = mkName $ "Vec" ++ [toUpper c]
	instanceD (sequence
		[ [t| OfVectorType $(varT v) |]
		, [t| $(conT vc) $(varT v) |]
		]) [t| $(conT vc) (Node $(varT v)) |]
		[ funD (mkName $ [c, '_']) [clause [] (normalB [| ComponentNode (valueType (undefined :: $(varT v))) (valueType (undefined :: VecElement $(varT v))) $(litE $ charL c) |]) []]
		]

{- instance
	( OfVectorType v
	, OfVectorType (SwizzleVec{X..W}{1..4}Result v)
	, SwizzleVec{X..W}{1..4} v
	) => SwizzleVec{X..W}{1..4} (Node v)
-}
forM [(maxComp, dim) | maxComp <- [1..4], dim <- [1..4]] $ \(maxComp, dim) -> do
	v <- newName "v"
	let components = take maxComp "xyzw"
	let nameSuffix = [toUpper $ last components, intToDigit dim]
	let sv = mkName $ "SwizzleVec" ++ nameSuffix
	let resultTypeName = mkName $ "SwizzleVecResult" ++ nameSuffix
	let variants = filter variantFilter $ genVariants dim where
		genVariants 0 = [""]
		genVariants len = [c : cs | c <- components, cs <- genVariants $ len - 1]
		variantFilter variant = all (\c -> elem c components) variant && elem (last components) variant
	let funDecl variant = do
		funD (mkName $ variant ++ "__") [clause [] (normalB [| SwizzleNode (valueType (undefined :: $(varT v))) (valueType (undefined :: $(conT resultTypeName) $(varT v))) $(litE $ stringL variant) |]) []]
	let resultTypeDecl = tySynInstD resultTypeName $ tySynEqn
		[ [t| Node $(varT v) |] ]
		[t| Node ($(conT resultTypeName) $(varT v)) |]
	instanceD (sequence
		[ [t| OfVectorType $(varT v) |]
		, [t| OfVectorType ($(conT $ mkName $ "SwizzleVecResult" ++ nameSuffix) $(varT v)) |]
		, [t| $(conT sv) $(varT v) |]
		])
		[t| $(conT sv) (Node $(varT v)) |] $ resultTypeDecl : map funDecl variants

---- Combine{2..4}Vec instances.
instance (OfValueType a, OfValueType b, Combine2Vec a b, OfValueType (Combine2VecResult a b)) => Combine2Vec (Node a) (Node b) where
	type Combine2VecResult (Node a) (Node b) = Node (Combine2VecResult a b)
	combine2Vec = Combine2VecNode
		(valueType (undefined :: a))
		(valueType (undefined :: b))
		(valueType (undefined :: Combine2VecResult a b))
instance (OfValueType a, OfValueType b, OfValueType c, Combine3Vec a b c, OfValueType (Combine3VecResult a b c)) => Combine3Vec (Node a) (Node b) (Node c) where
	type Combine3VecResult (Node a) (Node b) (Node c) = Node (Combine3VecResult a b c)
	combine3Vec = Combine3VecNode
		(valueType (undefined :: a))
		(valueType (undefined :: b))
		(valueType (undefined :: c))
		(valueType (undefined :: Combine3VecResult a b c))
instance (OfValueType a, OfValueType b, OfValueType c, OfValueType d, Combine4Vec a b c d, OfValueType (Combine4VecResult a b c d)) => Combine4Vec (Node a) (Node b) (Node c) (Node d) where
	type Combine4VecResult (Node a) (Node b) (Node c) (Node d) = Node (Combine4VecResult a b c d)
	combine4Vec = Combine4VecNode
		(valueType (undefined :: a))
		(valueType (undefined :: b))
		(valueType (undefined :: c))
		(valueType (undefined :: d))
		(valueType (undefined :: Combine4VecResult a b c d))

-- | Program monad.
type Program a = ReaderT (IORef State) IO a

runProgram :: Program () -> IO State
runProgram program = do
	stateVar <- newIORef State
		{ stateTemps = []
		, stateTempsCount = 0
		, stateStage = VertexStage
		, stateTargets = []
		}
	runReaderT program stateVar
	state@State
		{ stateStage = stage
		} <- readIORef stateVar
	if stage /= EndStage then fail "wrong program: stage should be end"
	else return ()
	return state
