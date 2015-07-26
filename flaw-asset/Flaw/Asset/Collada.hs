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
	, runCollada
	, initColladaCache
	, getElementById
	, getAllElementsByTag
	, parseTriangles
	, parseMesh
	, parseGeometry
	, parseNode
	, parseAnimation
	, animateNode
	, ColladaSkeleton()
	, parseSkeleton
	, animateSkeleton
	, chunks3
	, chunks3stride
	) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString.Lazy as BL
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
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

-- | Get "id" attribute of element.
getElementId :: XML.Element -> ColladaM String
getElementId = getElementAttr "id"

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
				unit <- getElementAttr "meter" unitElement
				state $ \cache@ColladaCache
					{ ccSettings = settings
					} -> ((), cache
					{ ccSettings = settings
						{ csUnit = read unit
						}
					})
			else return ()
			ignoreErrors $ do
				elementId <- getElementId element
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
		elementId <- getElementId element
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
		sourceRef <- getElementAttr "source" inputElement
		sourceElement <- resolveElement sourceRef
		parseSource sourceElement
	else do
		techniqueElement <- getSingleChildWithTag "technique_common" element
		accessorElement <- getSingleChildWithTag "accessor" techniqueElement
		count <- liftM parse $ getElementAttr "count" accessorElement
		stride <- liftM parse $ getElementAttr "stride" accessorElement
		sourceRef <- getElementAttr "source" accessorElement
		arrayElement <- resolveElement sourceRef
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
	sourceRef <- getElementAttr "source" inputElement
	sourceElement <- resolveElement sourceRef
	return ColladaInputTag
		{ citSemantic = semantic
		, citOffset = offset
		, citSourceElement = sourceElement
		}

type VertexConstructor q = (forall a v. Parse a v => String -> ColladaM (v a, Int)) -> ColladaM (V.Vector q)

parseTriangles :: VertexConstructor q -> XML.Element -> ColladaM (V.Vector q)
parseTriangles f element = do
	-- get count
	triangleCount <- liftM parse $ getElementAttr "count" element
	-- parse indices
	indices <- parseArray =<< getSingleChildWithTag "p" element
	-- parse inputs
	inputElements <- getChildrenWithTag "input" element
	inputs <- mapM parseInput inputElements
	-- calculate stride and count
	let stride = 1 + (maximum $ map citOffset inputs)
	let count = VG.length indices `div` stride
	-- calculate vertices
	vertices <- f $ \semantic -> do
		case filter (\i -> citSemantic i == semantic) inputs of
			[ColladaInputTag
				{ citOffset = offset
				, citSourceElement = sourceElement
				}] -> do
				(values, valuesStride) <- parseSource sourceElement
				let r = VG.generate (count * valuesStride) $ \k -> let
					(i, j) = k `divMod` valuesStride
					in values VG.! ((indices VG.! (i * stride + offset)) * valuesStride + j)
				return (r, valuesStride)
			_ -> throwError $ show ("parseTriangles: wrong semantic", semantic)
	-- take enough and flip triangles
	return $ flipTriangles $ V.take (triangleCount * 3) vertices

parseMesh :: VertexConstructor v -> XML.Element -> ColladaM (V.Vector v)
parseMesh f element = parseTriangles f =<< getSingleChildWithTag "triangles" element

parseGeometry :: VertexConstructor v -> XML.Element -> ColladaM (V.Vector v)
parseGeometry f element = parseMesh f =<< getSingleChildWithTag "mesh" element

-- | Transform.
data ColladaTransformTag
	= ColladaTranslateTag Vec3f
	| ColladaRotateTag Vec3f Float
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
	nodeId <- getElementId element
	sid <- getElementAttr "sid" element

	unit <- liftM (csUnit . ccSettings) get

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
		sourceRef <- getElementAttr "source" channelElement
		samplerElement <- resolveElement sourceRef
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

						Nothing -> return []
				-- resulting animation function
				return $ \time -> foldl' (\transformTag channelAnimator -> channelAnimator transformTag time) initialTransformTag channelAnimators

			Nothing -> return $ const initialTransformTag

		-- convert transform tag to transform
		return $ \time -> case transformTagAnimator time of
			ColladaTranslateTag offset -> transformTranslation offset
			ColladaRotateTag axis angle -> transformAxisRotation axis angle

	-- resulting function combines transforms from all transform animators
	return $ \time ->
		foldr (\transformTagAnimator transform ->
			combineTransform (transformTagAnimator time) transform) identityTransform transformTagAnimators

class Animatable a where
	animatableStride :: a -> Int
	animatableConstructor :: VU.Vector Float -> Int -> a
	interpolateAnimatable :: Float -> a -> a -> a

instance Animatable Float where
	animatableStride _ = 1
	animatableConstructor v offset = v VG.! offset
	interpolateAnimatable t a b = a * (1 - t) + b * t

instance Animatable (Vec3 Float) where
	animatableStride _ = 3
	animatableConstructor v offset = Vec3 (v VG.! offset) (v VG.! (offset + 1)) (v VG.! (offset + 2))
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

-- | Split vector into triples.
chunks3 :: VG.Vector v a => v a -> V.Vector (a, a, a)
chunks3 v = r where
	(len, 0) = (VG.length v) `divMod` 3
	r = V.generate len $ \i -> let k = i * 3 in (v VG.! k, v VG.! (k + 1), v VG.! (k + 2))

-- | Split vector with stride into triples (and check that stride is 3).
chunks3stride :: VG.Vector v a => (v a, Int) -> V.Vector (a, a, a)
chunks3stride (v, 3) = chunks3 v
chunks3stride (_, _) = error "stride is not 3"

-- | Flip triangles.
flipTriangles :: VG.Vector v a => v a -> v a
flipTriangles v = if len `mod` 3 == 0 then VG.generate len f else error "flipTriangles: not multiply of 3" where
	len = VG.length v
	f k = let (i, j) = k `divMod` 3 in v VG.! (i * 3 + p j)
	p j = case j of
		0 -> 1
		1 -> 0
		2 -> 2
		_ -> undefined
