{-|
Module: Flaw.Asset.Collada
Description: Collada support.
License: MIT
-}

{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, RankNTypes #-}

module Flaw.Asset.Collada
	( Parse()
	, ColladaM()
	, ColladaCache(..)
	, ColladaSettings(..)
	, getSingleChildWithTag
	, runCollada
	, initColladaCache
	, getElementById
	, getAllElementsByTag
	, ColladaVerticesData(..)
	, parseTriangles
	, parseMesh
	, parseGeometry
	, parseNode
	, parseAnimation
	, animateNode
	, ColladaSkeleton()
	, parseSkeleton
	, animateSkeleton
	, ColladaSkin(..)
	, ColladaBone(..)
	, parseSkin
	) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.ST
import Control.Monad.State
import qualified Data.ByteString.Lazy as BL
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Text.XML.Light as XML

import Flaw.Math
import Flaw.Math.Transform

data ColladaCache = ColladaCache
	{ ccSettings :: !ColladaSettings
	, ccContents :: [XML.Content]
	, ccElementsById :: !(Map.Map String XML.Element)
	, ccIntArrays :: !(Map.Map String (VU.Vector Int))
	, ccFloatArrays :: !(Map.Map String (VU.Vector Float))
	, ccNameArrays :: !(Map.Map String (V.Vector T.Text))
	}

data ColladaSettings = ColladaSettings
	{ csUnit :: Float
	, csUnitMat :: Mat4x4f
	, csInvUnitMat :: Mat4x4f
	}

type ColladaM a = StateT ColladaCache (Either String) a

-------- XML helpers.

tryGetElementAttr :: String -> XML.Element -> ColladaM (Maybe String)
tryGetElementAttr attrName XML.Element
	{ XML.elAttribs = attribs
	} = case filter (\XML.Attr { XML.attrKey = XML.QName { XML.qName = name } } -> name == attrName) attribs of
	[XML.Attr { XML.attrVal = val }] -> return $ Just val
	_ -> return Nothing

-- | Get attribute of element.
getElementAttr :: String -> XML.Element -> ColladaM String
getElementAttr attrName element = do
	maybeAttr <- tryGetElementAttr attrName element
	case maybeAttr of
		Just attr -> return attr
		Nothing -> throwError $ show ("no exactly one attribute", attrName)

-- | Get children elements with specified tag.
getChildrenWithTag :: String -> XML.Element -> ColladaM [XML.Element]
getChildrenWithTag tag XML.Element
	{ XML.elContent = contents
	} = return $ concat $ map f contents where
	f content = case content of
		XML.Elem element@XML.Element
			{ XML.elName = XML.QName
				{ XML.qName = name
				}
			} -> if name == tag then [element] else []
		_ -> []

getSingleChildWithTag :: String -> XML.Element -> ColladaM XML.Element
getSingleChildWithTag tag element = do
	children <- getChildrenWithTag tag element
	case children of
		[r] -> return r
		_ -> throwError $ show ("no exactly one child", tag)

runCollada :: ColladaM a -> Either String a
runCollada r = evalStateT r undefined

-- | Init collada cache.
initColladaCache :: BL.ByteString -> ColladaM ()
initColladaCache fileData = do
	let
		ignoreErrors q = catchError q (\_ -> return ())
		traverseElement element@XML.Element
			{ XML.elName = XML.QName
				{ XML.qName = tag
				}
			, XML.elContent = contents
			} = do
			if tag == "COLLADA" then ignoreErrors $ do
				assetElement <- getSingleChildWithTag "asset" element
				unitElement <- getSingleChildWithTag "unit" assetElement
				unit <- liftM read $ getElementAttr "meter" unitElement
				let invUnit = 1 / unit
				state $ \cache@ColladaCache
					{ ccSettings = settings
					} -> ((), cache
					{ ccSettings = settings
						{ csUnit = unit
						, csUnitMat = Mat4x4
							unit 0 0 0
							0 unit 0 0
							0 0 unit 0
							0 0 0 1
						, csInvUnitMat = Mat4x4
							invUnit 0 0 0
							0 invUnit 0 0
							0 0 invUnit 0
							0 0 0 1
						}
					})
			else return ()
			ignoreErrors $ do
				elementId <- getElementAttr "id" element
				cache <- get
				put $ cache
					{ ccElementsById = Map.insert elementId element $ ccElementsById cache
					}
			traverseContents contents
		traverseContent content = case content of
			XML.Elem element -> traverseElement element
			_ -> return ()
		traverseContents = mapM_ traverseContent
		fileContents = XML.parseXML fileData
	put ColladaCache
		{ ccSettings = ColladaSettings
			{ csUnit = 1
			, csUnitMat = identityTransform
			, csInvUnitMat = identityTransform
			}
		, ccContents = fileContents
		, ccElementsById = Map.empty
		, ccIntArrays = Map.empty
		, ccFloatArrays = Map.empty
		, ccNameArrays = Map.empty
		}
	traverseContents fileContents

-- | Get element by id.
getElementById :: String -> ColladaM XML.Element
getElementById name = do
	cache <- get
	case Map.lookup name $ ccElementsById cache of
		Just element -> return element
		Nothing -> throwError $ show ("no element", name)

-- | Get element by #id or local name.
resolveElement :: String -> ColladaM XML.Element
resolveElement name = case name of
	('#' : elementId) -> getElementById elementId
	_ -> throwError "local addresses not implemented yet" -- TODO: local addresses

-- | Get all elements by tag.
getAllElementsByTag :: String -> ColladaM [XML.Element]
getAllElementsByTag tag = do
	ColladaCache
		{ ccContents = rootContents
		} <- get
	let traverseContents contents = liftM concat $ forM contents $ \content -> case content of
		XML.Elem element@XML.Element
			{ XML.elName = XML.QName
				{ XML.qName = elementName
				}
			, XML.elContent = elementContents
			} -> do
			subElements <- traverseContents elementContents
			return $ if elementName == tag then element : subElements else subElements
		_ -> return []
	traverseContents rootContents

class VG.Vector v a => Parse a v | a -> v where
	parse :: String -> a
	getParsedArrays :: ColladaM (Map.Map String (v a))
	putParsedArrays :: (Map.Map String (v a) -> Map.Map String (v a)) -> ColladaM ()

instance Parse Int VU.Vector where
	parse = read
	getParsedArrays = liftM ccIntArrays get
	putParsedArrays f = do
		cache <- get
		put cache { ccIntArrays = f $ ccIntArrays cache }

instance Parse Float VU.Vector where
	parse = read
	getParsedArrays = liftM ccFloatArrays get
	putParsedArrays f = do
		cache <- get
		put cache { ccFloatArrays = f $ ccFloatArrays cache }

instance Parse T.Text V.Vector where
	parse = T.pack
	getParsedArrays = liftM ccNameArrays get
	putParsedArrays f = do
		cache <- get
		put cache { ccNameArrays = f $ ccNameArrays cache }

-- | Get contents of an element as CData, split into words and parse.
parseArrayUncached :: Parse a v => XML.Element -> ColladaM (v a)
parseArrayUncached element = case XML.elContent element of
	[XML.Text XML.CData
		{ XML.cdData = str
		}] -> return $ VG.fromList $ map parse $ words str
	_ -> throwError "wrong array"

-- | Get contents of an element as CData, split into words, parse and cache.
parseArray :: Parse a v => XML.Element -> ColladaM (v a)
parseArray element = catchError withId withoutId where
	withId = do
		elementId <- getElementAttr "id" element
		arrays <- getParsedArrays
		case Map.lookup elementId arrays of
			Just result -> return result
			Nothing -> do
				result <- parseArrayUncached element
				putParsedArrays $ Map.insert elementId result
				return result
	withoutId _err = parseArrayUncached element

-- | Parse "source" tag. Right now it just returns underlying array with stride.
parseSource :: Parse a v => XML.Element -> ColladaM (v a, Int)
parseSource element@XML.Element
	{ XML.elName = XML.QName
		{ XML.qName = name
		}
	} = do
	if name == "vertices" then do
		inputElement <- getSingleChildWithTag "input" element
		sourceElement <- resolveElement =<< getElementAttr "source" inputElement
		parseSource sourceElement
	else do
		techniqueElement <- getSingleChildWithTag "technique_common" element
		accessorElement <- getSingleChildWithTag "accessor" techniqueElement
		count <- liftM parse $ getElementAttr "count" accessorElement
		stride <- liftM parse $ getElementAttr "stride" accessorElement
		arrayElement <- resolveElement =<< getElementAttr "source" accessorElement
		values <- parseArray arrayElement
		return (VG.take (count * stride) values, stride)

-- | "Input" tag structure.
data ColladaInputTag = ColladaInputTag
	{ citSemantic :: String
	, citOffset :: Int
	, citSourceElement :: XML.Element
	}

-- | Parse "input" tag.
parseInput :: XML.Element -> ColladaM ColladaInputTag
parseInput inputElement = do
	semantic <- getElementAttr "semantic" inputElement
	offset <- liftM (maybe 0 parse) $ tryGetElementAttr "offset" inputElement
	sourceElement <- resolveElement =<< getElementAttr "source" inputElement
	return ColladaInputTag
		{ citSemantic = semantic
		, citOffset = offset
		, citSourceElement = sourceElement
		}

data ColladaVerticesData = ColladaVerticesData
	{ cvdCount :: !Int
	, cvdPositionIndices :: ColladaM (VU.Vector Int)
	, cvdPositions :: ColladaM (V.Vector Vec3f)
	, cvdNormals :: ColladaM (V.Vector Vec3f)
	, cvdTexcoords :: ColladaM (V.Vector Vec3f)
	, cvdBones :: ColladaM (V.Vector Vec4i)
	, cvdWeights :: ColladaM (V.Vector Vec4f)
	}

parseTriangles :: XML.Element -> ColladaM ColladaVerticesData
parseTriangles element = do
	-- get count
	trianglesCount <- liftM parse $ getElementAttr "count" element
	-- parse indices
	indices <- parseArray =<< getSingleChildWithTag "p" element
	-- parse inputs
	inputs <- mapM parseInput =<< getChildrenWithTag "input" element
	-- calculate stride and count
	let stride = 1 + (maximum $ map citOffset inputs)
	let count = VU.length indices `div` stride

	-- check
	if trianglesCount * 3 /= count
		then throwError "wrong number of triangles or indices"
		else return ()

	-- flip indices to fix vertex order in triangles
	let flippedIndices = VU.generate (count * stride) $ \i -> let
		(p, q) = i `divMod` stride
		(pp, pq) = p `divMod` 3
		f k = case k of
			0 -> 1
			1 -> 0
			2 -> 2
			_ -> undefined
		in indices VU.! ((pp * 3 + f pq) * stride + q)

	let stream semantic = case filter (\i -> citSemantic i == semantic) inputs of
		[ColladaInputTag
			{ citOffset = offset
			, citSourceElement = sourceElement
			}] -> do
			values <- parseStridables =<< parseSource sourceElement
			return $ VG.generate count $ \i -> values VG.! (flippedIndices VU.! (i * stride + offset))
		[] -> return VG.empty
		_ -> throwError $ show ("parseTriangles: wrong semantic", semantic)

	let positionIndices = case filter (\i -> citSemantic i == "VERTEX") inputs of
		[ColladaInputTag
			{ citOffset = offset
			}] -> return $ VU.generate count $ \i -> flippedIndices VU.! (i * stride + offset)
		_ -> throwError "no position indices"

	unit <- liftM (csUnit . ccSettings) get

	return ColladaVerticesData
		{ cvdCount = count
		, cvdPositionIndices = positionIndices
		, cvdPositions = liftM (V.map (* vecFromScalar unit)) $ stream "VERTEX"
		, cvdNormals = stream "NORMAL"
		, cvdTexcoords = stream "TEXCOORD"
		, cvdBones = return V.empty
		, cvdWeights = return V.empty
		}

parseMesh :: XML.Element -> ColladaM ColladaVerticesData
parseMesh element = parseTriangles =<< getSingleChildWithTag "triangles" element

parseGeometry :: XML.Element -> ColladaM ColladaVerticesData
parseGeometry element = parseMesh =<< getSingleChildWithTag "mesh" element

-- | Transform.
data ColladaTransformTag
	= ColladaTranslateTag Vec3f
	| ColladaRotateTag Vec3f Float
	| ColladaMatrixTag Mat4x4f
	deriving Show

-- | Node.
data ColladaNodeTag = ColladaNodeTag
	{ cntID :: String
	, cntSID :: String
	, cntTransforms :: [(Maybe String, ColladaTransformTag)]
	, cntSubnodes :: [ColladaNodeTag]
	} deriving Show

parseNode :: XML.Element -> ColladaM ColladaNodeTag
parseNode element@XML.Element
	{ XML.elContent = contents
	} = do
	nodeId <- liftM (maybe "" id) $ tryGetElementAttr "id" element
	sid <- liftM (maybe "" id) $ tryGetElementAttr "sid" element

	settings <- liftM ccSettings get
	let unit = csUnit settings
	let unitMat = csUnitMat settings
	let invUnitMat = csInvUnitMat settings

	-- traverse sub elements
	let f node@ColladaNodeTag
		{ cntTransforms = transforms
		, cntSubnodes = subnodes
		} content = do
		case content of
			XML.Elem subElement@XML.Element
				{ XML.elName = XML.QName
					{ XML.qName = subElementName
					}
				} -> do
				case subElementName of
					"node" -> do
						subnode <- parseNode subElement
						return node
							{ cntSubnodes = subnodes ++ [subnode]
							}
					"translate" -> do
						maybeTransformSID <- tryGetElementAttr "sid" subElement
						[x, y, z] <- liftM VG.toList $ parseArray subElement
						return node
							{ cntTransforms = transforms ++ [(maybeTransformSID, ColladaTranslateTag $ Vec3 x y z * vecFromScalar unit)]
							}
					"rotate" -> do
						maybeTransformSID <- tryGetElementAttr "sid" subElement
						[x, y, z, a] <- liftM VG.toList $ parseArray subElement
						return node
							{ cntTransforms = transforms ++ [(maybeTransformSID, ColladaRotateTag (Vec3 x y z) (a * pi / 180 :: Float))]
							}
					"matrix" -> do
						maybeTransformSID <- tryGetElementAttr "sid" subElement
						mat <- liftM ((flip constructStridable) 0) $ parseArray subElement
						return node
							{ cntTransforms = transforms ++ [(maybeTransformSID, ColladaMatrixTag (unitMat `mul` (mat :: Mat4x4f) `mul` invUnitMat))]
							}
					_ -> return node
			_ -> return node
	foldM f ColladaNodeTag
		{ cntID = nodeId
		, cntSID = sid
		, cntTransforms = []
		, cntSubnodes = []
		} contents

-- | Parse "sampler" tag.
-- Essentially just resolves INPUT and OUTPUT sources.
parseSampler :: (Parse i vi, Parse o vo) => XML.Element -> ColladaM ((vi i, Int), (vo o, Int))
parseSampler element = do
	inputElements <- getChildrenWithTag "input" element
	inputs <- mapM parseInput inputElements
	let getInput semantic = case filter (\i -> citSemantic i == semantic) inputs of
		[ColladaInputTag
			{ citOffset = 0
			, citSourceElement = sourceElement
			}] -> parseSource sourceElement
		_ -> throwError $ show ("parseSampler: wrong semantic", semantic)
	resultInputs <- getInput "INPUT"
	resultOutputs <- getInput "OUTPUT"
	return (resultInputs, resultOutputs)

-- | Animation is just a collection of channels.
newtype ColladaAnimation = ColladaAnimation [ColladaChannelTag]

-- | "Channel" tag structure.
data ColladaChannelTag = ColladaChannelTag
	{ cctTarget :: String
	, cctSamplerElement :: XML.Element
	} deriving Show

-- | Parse "animation" tag.
parseAnimation :: XML.Element -> ColladaM ColladaAnimation
parseAnimation element = do
	channelElements <- getChildrenWithTag "channel" element
	liftM ColladaAnimation $ forM channelElements $ \channelElement -> do
		samplerElement <- resolveElement =<< getElementAttr "source" channelElement
		target <- getElementAttr "target" channelElement
		return ColladaChannelTag
			{ cctTarget = target
			, cctSamplerElement = samplerElement
			}

-- | Create animation function for node.
animateNode :: Transform t => ColladaNodeTag -> ColladaAnimation -> ColladaM (Float -> t Float)
animateNode ColladaNodeTag
	{ cntID = nodeId
	, cntTransforms = transformTags
	} (ColladaAnimation channels) = do

	unit <- liftM (csUnit . ccSettings) get

	-- list of animators (one per transform tag)
	transformTagAnimators <- forM transformTags $ \(maybeName, initialTransformTag) -> do

		-- if transform tag is named, there might be some channels affecting it
		transformTagAnimator <- case maybeName of

			Just name -> do
				-- list of transform combinators for channels affecting transform
				channelAnimators <- liftM concat $ forM channels $ \ColladaChannelTag
					{ cctTarget = target
					, cctSamplerElement = samplerElement
					} -> case stripPrefix (nodeId ++ "/" ++ name) target of
						Just path -> case initialTransformTag of
							ColladaTranslateTag _initialOffset -> case path of

								"" -> do
									a <- animateSampler samplerElement
									return [\(ColladaTranslateTag _offset) time -> ColladaTranslateTag $ a time * vecFromScalar unit]

								".X" -> do
									a <- animateSampler samplerElement
									return [\(ColladaTranslateTag (Vec3 _x y z)) time -> ColladaTranslateTag $ Vec3 (a time * unit) y z]

								".Y" -> do
									a <- animateSampler samplerElement
									return [\(ColladaTranslateTag (Vec3 x _y z)) time -> ColladaTranslateTag $ Vec3 x (a time * unit) z]

								".Z" -> do
									a <- animateSampler samplerElement
									return [\(ColladaTranslateTag (Vec3 x y _z)) time -> ColladaTranslateTag $ Vec3 x y (a time * unit)]

								_ -> throwError $ "unknown path for translate tag: " ++ path

							ColladaRotateTag _initialAxis _initialAngle -> case path of

								".ANGLE" -> do
									a <- animateSampler samplerElement
									return [\(ColladaRotateTag axis _angle) time -> ColladaRotateTag axis (a time * pi / 180)]

								_ -> throwError $ "unknown path for rotate tag: " ++ path

							ColladaMatrixTag _initialMat -> case path of

								_ -> throwError $ "unknown path for matrix tag: " ++ path

						Nothing -> return []
				-- resulting animation function
				return $ \time -> foldl' (\transformTag channelAnimator -> channelAnimator transformTag time) initialTransformTag channelAnimators

			Nothing -> return $ const initialTransformTag

		-- convert transform tag to transform
		return $ \time -> case transformTagAnimator time of
			ColladaTranslateTag offset -> transformTranslation offset
			ColladaRotateTag axis angle -> transformAxisRotation axis angle
			ColladaMatrixTag mat -> transformFromMatrix mat

	-- resulting function combines transforms from all transform animators
	return $ \time ->
		foldr (\transformTagAnimator transform ->
			combineTransform (transformTagAnimator time) transform) identityTransform transformTagAnimators

class Stridable s where
	stridableStride :: s a -> Int
	constructStridable :: VG.Vector v a => v a -> Int -> s a

instance Stridable Vec2 where
	stridableStride _ = 2
	constructStridable v i = Vec2 (q 0) (q 1) where q j = v VG.! (i + j)

instance Stridable Vec3 where
	stridableStride _ = 3
	constructStridable v i = Vec3 (q 0) (q 1) (q 2) where q j = v VG.! (i + j)

instance Stridable Vec4 where
	stridableStride _ = 4
	constructStridable v i = Vec4 (q 0) (q 1) (q 2) (q 3) where q j = v VG.! (i + j)

instance Stridable Mat4x4 where
	stridableStride _ = 16
	constructStridable v i = Mat4x4
		(q 0) (q 1) (q 2) (q 3)
		(q 4) (q 5) (q 6) (q 7)
		(q 8) (q 9) (q 10) (q 11)
		(q 12) (q 13) (q 14) (q 15)
		where q j = v VG.! (i + j)

-- | Convert vector of primitive values to vector of Stridables.
stridableStream :: (Stridable s, VG.Vector v a) => v a -> V.Vector (s a)
stridableStream q = f undefined q where
	f :: (Stridable s, VG.Vector v a) => s a -> v a -> V.Vector (s a)
	f u v = V.generate (VG.length v `div` stride) $ \i -> constructStridable v $ i * stride where
		stride = stridableStride u

parseStridables :: (Stridable s, VG.Vector v a) => (v a, Int) -> ColladaM (V.Vector (s a))
parseStridables (q, stride) = f undefined q where
	f :: (Stridable s, VG.Vector v a) => s a -> v a -> ColladaM (V.Vector (s a))
	f u v = do
		if stride == stridableStride u
			then return $ stridableStream v
			else throwError "wrong stride"

class Animatable a where
	animatableStride :: a -> Int
	animatableConstructor :: VU.Vector Float -> Int -> a
	interpolateAnimatable :: Float -> a -> a -> a

instance Animatable Float where
	animatableStride _ = 1
	animatableConstructor v i = v VG.! i
	interpolateAnimatable t a b = a * (1 - t) + b * t

instance Animatable (Vec3 Float) where
	animatableStride _ = 3
	animatableConstructor v i = Vec3 (v VG.! i) (v VG.! (i + 1)) (v VG.! (i + 2))
	interpolateAnimatable t a b = a * vecFromScalar (1 - t) + b * vecFromScalar t

animateSampler :: Animatable a => XML.Element -> ColladaM (Float -> a)
animateSampler element = do
	((inputs, 1), (outputs, outputStride)) <- parseSampler element
	let len = VG.length inputs
	let search time left right = if left + 1 < right then let
		mid = (left + right) `div` 2
		midTime = inputs VG.! mid
		in if time >= midTime then search time mid right else search time left mid
		else left
	return $ \time -> let
		offset = search time 0 len
		offset2 = offset + 1
		input = inputs VG.! offset
		input2 = inputs VG.! offset2
		output = animatableConstructor outputs $ offset * outputStride
		output2 = animatableConstructor outputs $ offset2 * outputStride
		t = (time - input) / (input2 - input)
		in if offset + 1 >= len then output else interpolateAnimatable t output output2

-- | Flattened node hierarchy, in strict order from root to leaves.
newtype ColladaSkeleton = ColladaSkeleton (V.Vector ColladaSkeletonNode) deriving Show

data ColladaSkeletonNode = ColladaSkeletonNode
	{ csklnNodeTag :: ColladaNodeTag
	, csklnParentId :: !Int
	} deriving Show

-- | Create flat skeleton structure for node hierarchy.
parseSkeleton :: XML.Element -> ColladaM ColladaSkeleton
parseSkeleton element = do
	rootNodeTag <- parseNode element
	let go parentId currentId nodeTag@ColladaNodeTag
		{ cntSubnodes = subnodeTags
		} = (resultNextId, node : concat resultSubnodes) where
		node = ColladaSkeletonNode
			{ csklnNodeTag = nodeTag
			, csklnParentId = parentId
			}
		(resultNextId, resultSubnodes) = foldl (
			\(accNextId, accSubnodes) subnodeTag -> let
				(nextId, subnodes) = go currentId accNextId subnodeTag
				in (nextId, accSubnodes ++ [subnodes])
			) (currentId + 1, []) subnodeTags
	let (_, nodes) = go (-1) 0 rootNodeTag
	return $ ColladaSkeleton $ V.fromList nodes

-- | Create animation function for skeleton.
animateSkeleton :: Transform t => ColladaSkeleton -> ColladaAnimation -> ColladaM (t Float -> Float -> V.Vector (t Float))
animateSkeleton (ColladaSkeleton nodes) animation = do
	nodeAnimators <- forM nodes $ \ColladaSkeletonNode
		{ csklnNodeTag = nodeTag
		} -> animateNode nodeTag animation
	return $ \rootTransform time -> V.create $ do
		transforms <- VM.new $ V.length nodes
		forM_ [0..(V.length nodes - 1)] $ \i -> do
			let ColladaSkeletonNode
				{ csklnParentId = parentId
				} = nodes V.! i
			parentTransform <- if parentId >= 0 then VM.read transforms parentId else return rootTransform
			VM.write transforms i $ combineTransform parentTransform $ (nodeAnimators V.! i) time
		return transforms

data ColladaSkin t = ColladaSkin
	{
	-- | Bones used in skinning, in order corresponding to bone indices in mesh.
	  cskinBones :: !(V.Vector (ColladaBone t))
	} deriving Show

data ColladaBone t = ColladaBone
	{
	-- | Index of bone in skeleton.
	  cboneSkeletonIndex :: !Int
	-- | Inverse bind transform.
	, cboneInvBindTransform :: !t
	} deriving Show

parseSkin :: Transform t => ColladaSkeleton -> XML.Element -> ColladaM (ColladaVerticesData, ColladaSkin (t Float))
parseSkin (ColladaSkeleton nodes) skinElement = do
	settings <- liftM ccSettings get
	let unitMat = csUnitMat settings
	let invUnitMat = csInvUnitMat settings

	bindShapeTransform <- liftM (\v -> transformFromMatrix $ unitMat `mul` (constructStridable v 0 :: Mat4x4f) `mul` invUnitMat) (parseArray =<< getSingleChildWithTag "bind_shape_matrix" skinElement)

	jointsElement <- getSingleChildWithTag "joints" skinElement
	jointsInputs <- mapM parseInput =<< getChildrenWithTag "input" jointsElement

	let findInput inputs semantic parent = case filter (\input -> citSemantic input == semantic) inputs of
		[input] -> return input :: ColladaM ColladaInputTag
		_ -> throwError $ "no single " ++ parent ++ " input with " ++ semantic ++ " semantic"

	jointsJointInput <- findInput jointsInputs "JOINT" "joints"
	jointsJointNames <- liftM fst $ parseSource $ citSourceElement jointsJointInput
	jointsInvBindMatrixInput <- findInput jointsInputs "INV_BIND_MATRIX" "joints"
	jointsInvBindTransforms <- liftM (V.map $ \mat -> transformFromMatrix $ unitMat `mul` (mat :: Mat4x4f) `mul` invUnitMat) $ parseStridables =<< (parseSource $ citSourceElement jointsInvBindMatrixInput)

	skinBones <- forM (V.zip jointsJointNames jointsInvBindTransforms) $ \(jointName, jointInvBindTransform) -> do
		case V.findIndex (\ColladaSkeletonNode
			{ csklnNodeTag = ColladaNodeTag
				{ cntSID = sid
				}
			} -> T.pack sid == jointName) nodes of
			Just nodeIndex -> return ColladaBone
				{ cboneSkeletonIndex = nodeIndex
				, cboneInvBindTransform = combineTransform bindShapeTransform jointInvBindTransform
				}
			Nothing -> throwError $ "missing skeleton node for joint " ++ T.unpack jointName

	let namedBones = Map.fromList $ zip (V.toList jointsJointNames) [0..]

	vertexWeightsElement <- getSingleChildWithTag "vertex_weights" skinElement
	vertexWeightsInputs <- mapM parseInput =<< getChildrenWithTag "input" vertexWeightsElement
	let vertexWeightsStride = length vertexWeightsInputs

	vertexWeightsJointInput <- findInput vertexWeightsInputs "JOINT" "vertex_weights"
	vertexWeightsJointNames <- liftM fst $ parseSource $ citSourceElement vertexWeightsJointInput
	let vertexWeightsJointOffset = citOffset vertexWeightsJointInput
	vertexWeightsJointBones <- liftM V.convert $ V.forM vertexWeightsJointNames $ \jointName -> case Map.lookup jointName namedBones of
		Just bone -> return bone
		Nothing -> throwError $ "missing bone for joint " ++ T.unpack jointName

	vertexWeightsWeightInput <- findInput vertexWeightsInputs "WEIGHT" "vertex_weights"
	vertexWeightsWeights <- liftM fst $ parseSource $ citSourceElement vertexWeightsWeightInput
	let vertexWeightsWeightOffset = citOffset vertexWeightsWeightInput

	count <- liftM parse $ getElementAttr "count" vertexWeightsElement
	vcount <- parseArray =<< getSingleChildWithTag "vcount" vertexWeightsElement
	v <- parseArray =<< getSingleChildWithTag "v" vertexWeightsElement

	-- constant
	let bonesPerVertex = 4

	let (rawWeights, rawBones) = runST $ do
		weights <- VUM.new $ count * bonesPerVertex
		bones <- VUM.new $ count * bonesPerVertex
		let
			sums ps (s:ss) = ps : sums (ps + s) ss
			sums _ [] = []
		let is = [0..(count - 1)]
		let vcountList = VU.toList vcount
		-- loop for vertices
		forM_ (zip3 is vcountList $ sums 0 vcountList) $ \(i, bonesCount, j) -> do

			weightJointPairs <- VM.new bonesCount

			-- loop for bones of vertex
			forM_ [0..(bonesCount - 1)] $ \k -> do
				let o = (j + k) * vertexWeightsStride
				let jointIndex = v VU.! (o + vertexWeightsJointOffset)
				let weightIndex = v VU.! (o + vertexWeightsWeightOffset)
				VM.write weightJointPairs k (vertexWeightsWeights VU.! weightIndex, vertexWeightsJointBones VU.! jointIndex)

			-- sort weight-joint pairs
			VAI.sort weightJointPairs

			-- pick up most weighted
			freezedWeightJointPairs <- V.unsafeFreeze weightJointPairs
			let len = V.length freezedWeightJointPairs
			let bestWeightJointPairs =
				if len >= bonesPerVertex then V.drop (len - bonesPerVertex) freezedWeightJointPairs
				else freezedWeightJointPairs V.++ (V.fromList $ replicate (bonesPerVertex - len) (0, 0))

			-- calc sum of weights to normalize
			let weightSum = V.sum $ V.map fst bestWeightJointPairs

			-- write weights and bones
			forM_ [0..(bonesPerVertex - 1)] $ \k -> do
				let (weight, bone) = bestWeightJointPairs V.! k
				VUM.write weights (i * bonesPerVertex + k) $ weight / weightSum
				VUM.write bones (i * bonesPerVertex + k) bone

		freezedWeights <- VU.unsafeFreeze weights
		freezedBones <- VU.unsafeFreeze bones
		return (stridableStream freezedWeights, stridableStream freezedBones)

	verticesData <- parseGeometry =<< resolveElement =<< getElementAttr "source" skinElement

	positionIndices <- cvdPositionIndices verticesData

	return (verticesData
		{ cvdBones = return $ V.generate (VU.length positionIndices) $ \i -> rawBones V.! (positionIndices VU.! i)
		, cvdWeights = return $ V.generate (VU.length positionIndices) $ \i -> rawWeights V.! (positionIndices VU.! i)
		}, ColladaSkin
		{ cskinBones = skinBones
		})
