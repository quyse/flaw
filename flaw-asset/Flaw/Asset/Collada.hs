{-|
Module: Flaw.Asset.Collada
Description: Collada support.
License: MIT
-}

{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, RankNTypes #-}

module Flaw.Asset.Collada
	( Parse()
	, ColladaM()
	, ColladaSettings(..)
	, runCollada
	, initColladaCache
	, getElementById
	, parseTriangles
	, parseMesh
	, parseGeometry
	, chunks3
	, chunks3stride
	) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString.Lazy as BL
import Data.List
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified Text.XML.Light as XML

data ColladaCache = ColladaCache
	{ ccSettings :: ColladaSettings
	, ccContents :: [XML.Content]
	, ccElementsById :: Map.Map String XML.Element
	, ccIntArrays :: Map.Map String (VU.Vector Int)
	, ccFloatArrays :: Map.Map String (VU.Vector Float)
	, ccNameArrays :: Map.Map String (V.Vector T.Text)
	}

data ColladaSettings = ColladaSettings
	{ colladaUnit :: Float
	}

type ColladaM a = StateT ColladaCache (Either String) a

-------- XML helpers.

-- | Get attribute of element.
getElementAttr :: String -> XML.Element -> ColladaM String
getElementAttr attrName XML.Element
	{ XML.elAttribs = attribs
	} = case filter (\XML.Attr { XML.attrKey = XML.QName { XML.qName = name } } -> name == attrName) attribs of
	[XML.Attr { XML.attrVal = val }] -> return val
	_ -> throwError $ show ("no exactly one attribute", attrName)

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
						{ colladaUnit = read unit
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
			{ colladaUnit = 1
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

type VertexConstructor q = ColladaSettings -> (forall a v. Parse a v => String -> ColladaM (v a, Int)) -> ColladaM (V.Vector q)

parseTriangles :: VertexConstructor q -> XML.Element -> ColladaM (V.Vector q)
parseTriangles f element = do
	-- get count
	triangleCount <- liftM parse $ getElementAttr "count" element
	-- parse indices
	indices <- parseArray =<< getSingleChildWithTag "p" element
	-- parse inputs
	inputElements <- getChildrenWithTag "input" element
	inputs <- forM inputElements $ \inputElement -> do
		semantic <- getElementAttr "semantic" inputElement
		offset <- liftM parse $ getElementAttr "offset" inputElement
		sourceRef <- getElementAttr "source" inputElement
		sourceElement <- resolveElement sourceRef
		return (semantic, offset, sourceElement)
	-- calculate stride and count
	let stride = 1 + (maximum $ map (\(_s, o, _se) -> o) inputs)
	let count = VG.length indices `div` stride
	-- calculate vertices
	ColladaCache { ccSettings = settings } <- get
	vertices <- f settings $ \semantic -> do
		case find (\(s, _o, _se) -> s == semantic) inputs of
			Just (_s, o, se) -> do
				(a, as) <- parseSource se
				--return $ map (\i -> VG.toList $ a VG.! (indices VG.! i)) [o, (o + stride)..]
				let r = VG.generate (count * as) $ \q -> let
					(i, j) = q `divMod` as
					in a VG.! ((indices VG.! (i * stride + o)) * as + j)
				return (r, as)
			Nothing -> throwError $ show ("missing semantic", semantic)
	-- take enough and flip triangles
	return $ flipTriangles $ V.take (triangleCount * 3) vertices

parseMesh :: VertexConstructor v -> XML.Element -> ColladaM (V.Vector v)
parseMesh f element = parseTriangles f =<< getSingleChildWithTag "triangles" element

parseGeometry :: VertexConstructor v -> XML.Element -> ColladaM (V.Vector v)
parseGeometry f element = parseMesh f =<< getSingleChildWithTag "mesh" element

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
