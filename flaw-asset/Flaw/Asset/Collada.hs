{-|
Module: Flaw.Asset.Collada
Description: Collada support.
License: MIT
-}

{-# LANGUAGE RankNTypes #-}

module Flaw.Asset.Collada
	( Parse()
	, ColladaM()
	, runCollada
	, initColladaCache
	, getElementById
	, parseTriangles
	, parseMesh
	, parseGeometry
	) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Array((!), listArray)
import Data.List
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import qualified Text.XML.Light as XML

data Collada = Collada
	{ colladaUnit :: Float
	} deriving Show

data ColladaCache = ColladaCache
	{ ccContents :: [XML.Content]
	, ccElementsById :: Map.Map String XML.Element
	, ccIntArrays :: Map.Map String [Int]
	, ccFloatArrays :: Map.Map String [Float]
	, ccNameArrays :: Map.Map String [T.Text]
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
			{ XML.elContent = contents
			} = do
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
		{ ccContents = fileContents
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

class Parse a where
	parse :: String -> a
	getParsedArrays :: ColladaM (Map.Map String [a])
	putParsedArrays :: (Map.Map String [a] -> Map.Map String [a]) -> ColladaM ()

instance Parse Int where
	parse = read
	getParsedArrays = liftM ccIntArrays get
	putParsedArrays f = do
		cache <- get
		put cache { ccIntArrays = f $ ccIntArrays cache }

instance Parse Float where
	parse = read
	getParsedArrays = liftM ccFloatArrays get
	putParsedArrays f = do
		cache <- get
		put cache { ccFloatArrays = f $ ccFloatArrays cache }

instance Parse T.Text where
	parse = T.pack
	getParsedArrays = liftM ccNameArrays get
	putParsedArrays f = do
		cache <- get
		put cache { ccNameArrays = f $ ccNameArrays cache }

parseArrayUncached :: Parse a => XML.Element -> ColladaM [a]
parseArrayUncached element = case XML.elContent element of
	[XML.Text XML.CData
		{ XML.cdData = str
		}] -> return $ map parse $ words str
	_ -> throwError "wrong array"

parseArray :: Parse a => XML.Element -> ColladaM [a]
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

-- | Parse "source" tag. Right now it just returns contents of underlying array.
parseSource :: Parse a => XML.Element -> ColladaM [[a]]
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
		let gr v = let (a, b) = splitAt stride v in a : gr b
		return $ take count $ gr values

type VertexConstructor v = (forall a. Parse a => String -> ColladaM [[a]]) -> ColladaM [v]

parseTriangles :: VertexConstructor v -> XML.Element -> ColladaM [v]
parseTriangles f element = do
	-- helper to convert array to list
	let l2a list = listArray (0, (length list) - 1) list
	-- get count
	triangleCount <- liftM parse $ getElementAttr "count" element
	-- parse indices
	indices <- liftM l2a $ parseArray =<< getSingleChildWithTag "p" element
	-- parse inputs
	inputElements <- getChildrenWithTag "input" element
	inputs <- forM inputElements $ \inputElement -> do
		semantic <- getElementAttr "semantic" inputElement
		offset <- liftM parse $ getElementAttr "offset" inputElement
		sourceRef <- getElementAttr "source" inputElement
		sourceElement <- resolveElement sourceRef
		return (semantic, offset, sourceElement)
	-- calculate stride
	let stride = 1 + (maximum $ map (\(_s, o, _se) -> o) inputs)
	-- calculate vertices
	vertices <- f $ \semantic -> do
		case find (\(s, _o, _se) -> s == semantic) inputs of
			Just (_s, o, se) -> do
				a <- liftM l2a $ parseSource se
				return $ map (\i -> a ! (indices ! i)) [o, (o + stride)..]
			Nothing -> throwError $ show ("missing semantic", semantic)
	-- take enough and flip triangles
	return $ flipTriangles $ take (triangleCount * 3) vertices

parseMesh :: VertexConstructor v -> XML.Element -> ColladaM [v]
parseMesh f element = parseTriangles f =<< getSingleChildWithTag "triangles" element

parseGeometry :: VertexConstructor v -> XML.Element -> ColladaM [v]
parseGeometry f element = parseMesh f =<< getSingleChildWithTag "mesh" element

-- | Flip triangles.
flipTriangles :: [v] -> [v]
flipTriangles (v0:v1:v2:vs) = v1:v0:v2:flipTriangles vs
flipTriangles [] = []
flipTriangles _ = undefined
