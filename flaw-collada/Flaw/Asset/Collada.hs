{-|
Module: Flaw.Asset.Collada
Description: Collada support.
License: MIT
-}

{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, OverloadedStrings, RankNTypes #-}

module Flaw.Asset.Collada
  ( Parse()
  , ColladaM()
  , ColladaCache(..)
  , ColladaSettings(..)
  , tryGetElementAttr
  , getElementAttr
  , getChildrenWithTag
  , getSingleChildWithTag
  , runCollada
  , initColladaCache
  , tryGetElementById
  , getElementById
  , resolveElement
  , getAllElementsByTag
  , ColladaVerticesData(..)
  , parseTrianglesOrPolyList
  , parseMesh
  , parseGeometry
  , ColladaNodeTag(..)
  , parseNode
  , nullAnimation
  , parseAnimation
  , animateNode
  , ColladaSkeleton(..)
  , ColladaSkeletonNode(..)
  , parseSkeleton
  , animateSkeleton
  , ColladaSkin(..)
  , ColladaBone(..)
  , parseSkin
  , ColladaElement
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.State
import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.Maybe
import qualified Data.Map.Lazy as ML
import qualified Data.Map.Strict as MS
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word
import Foreign.Storable
import System.IO.Unsafe
import qualified Text.XML as XML

import Flaw.Math
import Flaw.Math.Transform

data ColladaCache = ColladaCache
  { ccSettings :: !ColladaSettings
  , ccRootElement :: !XML.Element
  , ccElementsById :: !(MS.Map T.Text XML.Element)
  , ccWord32Arrays :: !(MS.Map T.Text (VS.Vector Word32))
  , ccFloatArrays :: !(MS.Map T.Text (VS.Vector Float))
  , ccNameArrays :: !(MS.Map T.Text (V.Vector T.Text))
  }

data ColladaSettings = ColladaSettings
  { csUnit :: Float
  , csUnitMat :: Float4x4
  , csInvUnitMat :: Float4x4
  }

type ColladaM a = StateT ColladaCache (Either T.Text) a

instance MonadFail (Either T.Text) where
  fail = Left . T.pack

-------- XML helpers.

tryGetElementAttr :: T.Text -> XML.Element -> Maybe T.Text
tryGetElementAttr attrName XML.Element
  { XML.elementAttributes = attributes
  } = ML.lookup XML.Name
  { XML.nameLocalName = attrName
  , XML.nameNamespace = Nothing
  , XML.namePrefix = Nothing
  } attributes

-- | Get attribute of element.
getElementAttr :: T.Text -> XML.Element -> ColladaM T.Text
getElementAttr attrName element = case tryGetElementAttr attrName element of
  Just attr -> return attr
  Nothing -> throwError $ "no exactly one attribute: " <> attrName

-- | Get children elements with specified tag.
getChildrenWithTag :: T.Text -> XML.Element -> ColladaM [XML.Element]
getChildrenWithTag tag XML.Element
  { XML.elementNodes = nodes
  } = return $ concatMap f nodes where
  f node = case node of
    XML.NodeElement element@XML.Element
      { XML.elementName = XML.Name
        { XML.nameLocalName = name
        }
      } -> [element | name == tag]
    _ -> []

getSingleChildWithTag :: T.Text -> XML.Element -> ColladaM XML.Element
getSingleChildWithTag tag element = do
  children <- getChildrenWithTag tag element
  case children of
    [r] -> return r
    _ -> throwError $ "no exactly one child: " <> tag

runCollada :: ColladaM a -> Either T.Text a
runCollada r = evalStateT r undefined

-- | Init collada cache.
initColladaCache :: BL.ByteString -> ColladaM ()
initColladaCache fileData = do
  XML.Document
    { XML.documentRoot = rootElement
    } <- case XML.parseLBS XML.def fileData of
    Left e -> throwError $ "parse collada XML: " <> T.pack (show e)
    Right document -> return document
  put ColladaCache
    { ccSettings = ColladaSettings
      { csUnit = 1
      , csUnitMat = identityTransform
      , csInvUnitMat = identityTransform
      }
    , ccRootElement = rootElement
    , ccElementsById = MS.empty
    , ccWord32Arrays = MS.empty
    , ccFloatArrays = MS.empty
    , ccNameArrays = MS.empty
    }
  traverseElement rootElement
  where
    ignoreErrors q = catchError q (\_ -> return ())
    traverseElement element@XML.Element
      { XML.elementName = XML.Name
        { XML.nameLocalName = tag
        }
      , XML.elementNodes = nodes
      } = do
      when (tag == "COLLADA") $ ignoreErrors $ do
        assetElement <- getSingleChildWithTag "asset" element
        unitElement <- getSingleChildWithTag "unit" assetElement
        unit <- (read . T.unpack) <$> getElementAttr "meter" unitElement
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
      ignoreErrors $ do
        elementId <- getElementAttr "id" element
        cache <- get
        put $ cache
          { ccElementsById = MS.insert elementId element $ ccElementsById cache
          }
      mapM_ traverseNode nodes
    traverseNode node = case node of
      XML.NodeElement element -> traverseElement element
      _ -> return ()

-- | Try to get element by id.
tryGetElementById :: T.Text -> ColladaM (Maybe XML.Element)
tryGetElementById name = MS.lookup name . ccElementsById <$> get

-- | Get element by id.
getElementById :: T.Text -> ColladaM XML.Element
getElementById name = do
  cache <- get
  case MS.lookup name $ ccElementsById cache of
    Just element -> return element
    Nothing -> throwError $ "no element: " <> name

-- | Get element by #id or local name.
resolveElement :: T.Text -> ColladaM XML.Element
resolveElement name = case T.stripPrefix "#" name of
  Just elementId -> getElementById elementId
  Nothing -> throwError "local addresses not implemented yet" -- TODO: local addresses

-- | Get all elements by tag.
getAllElementsByTag :: T.Text -> ColladaM [XML.Element]
getAllElementsByTag tag = do
  ColladaCache
    { ccRootElement = rootElement
    } <- get
  return $ traverseElement rootElement []
  where
    traverseElement element@XML.Element
      { XML.elementName = XML.Name
        { XML.nameLocalName = elementName
        }
      , XML.elementNodes = nodes
      } results = if elementName == tag then element : nodesResults else nodesResults where
      nodesResults = foldr traverseNode results nodes
    traverseNode node results = case node of
      XML.NodeElement element -> traverseElement element results
      _ -> results

class Parse a where
  parse :: T.Text -> a

class (Parse a, VG.Vector v a) => ParseArray a v | a -> v where
  getParsedArrays :: ColladaM (MS.Map T.Text (v a))
  putParsedArrays :: (MS.Map T.Text (v a) -> MS.Map T.Text (v a)) -> ColladaM ()

instance Parse Int where
  parse = read . T.unpack

instance Parse Word32 where
  parse = read . T.unpack
instance ParseArray Word32 VS.Vector where
  getParsedArrays = ccWord32Arrays <$> get
  putParsedArrays f = do
    cache <- get
    put cache { ccWord32Arrays = f $ ccWord32Arrays cache }

instance Parse Float where
  parse = read . T.unpack
instance ParseArray Float VS.Vector where
  getParsedArrays = ccFloatArrays <$> get
  putParsedArrays f = do
    cache <- get
    put cache { ccFloatArrays = f $ ccFloatArrays cache }

instance Parse T.Text where
  parse = id
instance ParseArray T.Text V.Vector where
  getParsedArrays = ccNameArrays <$> get
  putParsedArrays f = do
    cache <- get
    put cache { ccNameArrays = f $ ccNameArrays cache }

-- | Get contents of an element as CData, split into words and parse.
parseArrayUncached :: ParseArray a v => XML.Element -> ColladaM (v a)
parseArrayUncached element = case XML.elementNodes element of
  [XML.NodeContent content] -> return $ VG.fromList $ map parse $ T.words content
  _ -> throwError "wrong array"

-- | Get contents of an element as CData, split into words, parse and cache.
parseArray :: ParseArray a v => XML.Element -> ColladaM (v a)
parseArray element = catchError withId withoutId where
  withId = do
    elementId <- getElementAttr "id" element
    arrays <- getParsedArrays
    case MS.lookup elementId arrays of
      Just result -> return result
      Nothing -> do
        result <- parseArrayUncached element
        putParsedArrays $ MS.insert elementId result
        return result
  withoutId _err = parseArrayUncached element

-- | Parse "source" tag. Right now it just returns underlying array with stride.
parseSource :: ParseArray a v => XML.Element -> ColladaM (v a, Int)
parseSource element@XML.Element
  { XML.elementName = XML.Name
    { XML.nameLocalName = name
    }
  } =
  if name == "vertices" then do
    inputElement <- getSingleChildWithTag "input" element
    sourceElement <- resolveElement =<< getElementAttr "source" inputElement
    parseSource sourceElement
  else do
    techniqueElement <- getSingleChildWithTag "technique_common" element
    accessorElement <- getSingleChildWithTag "accessor" techniqueElement
    count <- parse <$> getElementAttr "count" accessorElement
    stride <- parse <$> getElementAttr "stride" accessorElement
    arrayElement <- resolveElement =<< getElementAttr "source" accessorElement
    values <- parseArray arrayElement
    return (VG.take (count * stride) values, stride)

-- | "Input" tag structure.
data ColladaInputTag = ColladaInputTag
  { citSemantic :: !T.Text
  , citOffset :: {-# UNPACK #-} !Int
  , citSourceElement :: !XML.Element
  }

-- | Parse "input" tag.
parseInput :: XML.Element -> ColladaM ColladaInputTag
parseInput inputElement = do
  semantic <- getElementAttr "semantic" inputElement
  let offset = maybe 0 parse $ tryGetElementAttr "offset" inputElement
  sourceElement <- resolveElement =<< getElementAttr "source" inputElement
  return ColladaInputTag
    { citSemantic = semantic
    , citOffset = offset
    , citSourceElement = sourceElement
    }

data ColladaVerticesData = ColladaVerticesData
  { cvdCount :: {-# UNPACK #-} !Int
  , cvdPositionIndices :: ColladaM (VS.Vector Word32)
  , cvdPositions :: ColladaM (VS.Vector Float3)
  , cvdNormals :: ColladaM (VS.Vector Float3)
  , cvdTexcoords :: ColladaM (VS.Vector Float2)
  , cvdWeights :: ColladaM (VS.Vector Float4)
  , cvdBones :: ColladaM (VS.Vector (Vec4 Word8))
  }

parseTrianglesOrPolyList :: XML.Element -> ColladaM ColladaVerticesData
parseTrianglesOrPolyList element = do
  -- get count
  trianglesCount <- parse <$> getElementAttr "count" element
  -- if it's polylist, it has "vcount" element with numbers of vertices in each polygon
  -- check that there're only triangles
  vcountElements <- getChildrenWithTag "vcount" element
  case vcountElements of
    [vcountElement] -> do
      vcounts <- parseArray vcountElement
      if VG.length vcounts /= trianglesCount then throwError "wrong number of vcounts"
      else unless (VG.all (== (3 :: Word32)) vcounts) $ throwError "only triangles are supported"
    [] -> return ()
    _ -> throwError "must be 0 or 1 vcount element"
  -- parse indices
  indices <- parseArray =<< getSingleChildWithTag "p" element
  -- parse inputs
  inputs <- mapM parseInput =<< getChildrenWithTag "input" element
  -- calculate stride and count
  let stride = 1 + maximum (map citOffset inputs)
  let count = VG.length indices `quot` stride

  -- check
  when (trianglesCount * 3 /= count) $ throwError "wrong number of triangles or indices"

  -- flip indices to fix vertex order in triangles
  let
    flippedIndices = VS.generate (count * stride) $ \i -> let
      (p, q) = i `divMod` stride
      (pp, pq) = p `divMod` 3
      f k = case k of
        0 -> 1
        1 -> 0
        2 -> 2
        _ -> undefined
      in indices VG.! ((pp * 3 + f pq) * stride + q)

    stream semantic = case filter (\i -> citSemantic i == semantic) inputs of
      [ColladaInputTag
        { citOffset = offset
        , citSourceElement = sourceElement
        }] -> do
        values <- parseStridables =<< parseSource sourceElement
        return $ VG.generate count $ \i -> values VG.! fromIntegral (flippedIndices VG.! (i * stride + offset))
      [] -> return VG.empty
      _ -> throwError $ "parseTrianglesOrPolyList: wrong semantic: " <> semantic

    positionIndices = case filter (\i -> citSemantic i == "VERTEX") inputs of
      [ColladaInputTag
        { citOffset = offset
        }] -> return $ VG.generate count $ \i -> flippedIndices VG.! (i * stride + offset)
      _ -> throwError "no position indices"

  unit <- (csUnit . ccSettings) <$> get

  -- special handling of texcoord streams: allow both 2-coord and 3-coord streams
  let
    texcoords = catchError (stream "TEXCOORD") $ \_e -> VG.map (\(Vec3 x y _z) -> Vec2 x y) <$> stream "TEXCOORD"

  return ColladaVerticesData
    { cvdCount = count
    , cvdPositionIndices = positionIndices
    , cvdPositions = VG.map (* vecFromScalar unit) <$> stream "VERTEX"
    , cvdNormals = stream "NORMAL"
    , cvdTexcoords = texcoords
    , cvdWeights = return VG.empty
    , cvdBones = return VG.empty
    }

parseMesh :: XML.Element -> ColladaM ColladaVerticesData
parseMesh element = parseTrianglesOrPolyList =<< do
  -- get triangles or polylist tag
  trianglesElements <- getChildrenWithTag "triangles" element
  case trianglesElements of
    [trianglesElement] -> return trianglesElement
    [] -> getSingleChildWithTag "polylist" element
    _ -> throwError "must be 0 or 1 triangles element"

parseGeometry :: XML.Element -> ColladaM ColladaVerticesData
parseGeometry element = parseMesh =<< getSingleChildWithTag "mesh" element

-- | Transform.
data ColladaTransformTag
  = ColladaTranslateTag Float3
  | ColladaRotateTag Float3 Float
  | ColladaMatrixTag Float4x4
  deriving Show

-- | Node.
data ColladaNodeTag = ColladaNodeTag
  { cntElement :: !XML.Element
  , cntID :: !T.Text
  , cntSID :: !T.Text
  , cntTransforms :: [(Maybe T.Text, ColladaTransformTag)]
  , cntSubnodes :: [ColladaNodeTag]
  } deriving Show

parseNode :: XML.Element -> ColladaM ColladaNodeTag
parseNode element@XML.Element
  { XML.elementNodes = elementNodes
  } = do
  let
    nodeId = fromMaybe "" $ tryGetElementAttr "id" element
    sid = fromMaybe "" $ tryGetElementAttr "sid" element

  settings <- ccSettings <$> get

  let
    unit = csUnit settings
    unitMat = csUnitMat settings
    invUnitMat = csInvUnitMat settings

    -- traverse sub elements
    f node@ColladaNodeTag
      { cntTransforms = transforms
      , cntSubnodes = subnodes
      } elementNode = case elementNode of
      XML.NodeElement subElement@XML.Element
        { XML.elementName = XML.Name
          { XML.nameLocalName = subElementName
          }
        } -> case subElementName of
        "node" -> do
          subnode <- parseNode subElement
          return node
            { cntSubnodes = subnodes ++ [subnode]
            }
        "translate" -> do
          let maybeTransformSID = tryGetElementAttr "sid" subElement
          Vec3 x y z <- (`constructStridable` 0) <$> parseArray subElement
          return node
            { cntTransforms = transforms ++ [(maybeTransformSID, ColladaTranslateTag $ Vec3 x y z * vecFromScalar unit)]
            }
        "rotate" -> do
          let maybeTransformSID = tryGetElementAttr "sid" subElement
          Vec4 x y z a <- (`constructStridable` 0) <$> parseArray subElement
          return node
            { cntTransforms = transforms ++ [(maybeTransformSID, ColladaRotateTag (Vec3 x y z) (a * pi / 180 :: Float))]
            }
        "matrix" -> do
          let maybeTransformSID = tryGetElementAttr "sid" subElement
          mat <- (`constructStridable` 0) <$> parseArray subElement
          return node
            { cntTransforms = transforms ++ [(maybeTransformSID, ColladaMatrixTag (unitMat `mul` (mat :: Float4x4) `mul` invUnitMat))]
            }
        _ -> return node
      _ -> return node

  foldM f ColladaNodeTag
    { cntElement = element
    , cntID = nodeId
    , cntSID = sid
    , cntTransforms = []
    , cntSubnodes = []
    } elementNodes

-- | Parse "sampler" tag.
-- Essentially just resolves INPUT and OUTPUT sources.
parseSampler :: (ParseArray i vi, ParseArray o vo) => XML.Element -> ColladaM ((vi i, Int), (vo o, Int))
parseSampler element = do
  inputElements <- getChildrenWithTag "input" element
  inputs <- mapM parseInput inputElements
  let
    getInput semantic = case filter (\i -> citSemantic i == semantic) inputs of
      [ColladaInputTag
        { citOffset = 0
        , citSourceElement = sourceElement
        }] -> parseSource sourceElement
      _ -> throwError $ "parseSampler: wrong semantic: " <> semantic
  resultInputs <- getInput "INPUT"
  resultOutputs <- getInput "OUTPUT"
  return (resultInputs, resultOutputs)

-- | Animation is just a collection of channels.
newtype ColladaAnimation = ColladaAnimation [ColladaChannelTag]

-- | Empty animation.
nullAnimation :: ColladaAnimation
nullAnimation = ColladaAnimation []

-- | "Channel" tag structure.
data ColladaChannelTag = ColladaChannelTag
  { cctTarget :: !T.Text
  , cctSamplerElement :: !XML.Element
  } deriving Show

-- | Parse "animation" tag.
parseAnimation :: XML.Element -> ColladaM ColladaAnimation
parseAnimation element = do
  channelElements <- getChildrenWithTag "channel" element
  fmap ColladaAnimation $ forM channelElements $ \channelElement -> do
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

  ColladaSettings
    { csUnit = unit
    , csUnitMat = unitMat
    , csInvUnitMat = invUnitMat
    } <- fmap ccSettings get

  -- list of animators (one per transform tag)
  transformTagAnimators <- forM transformTags $ \(maybeName, initialTransformTag) -> do

    -- if transform tag is named, there might be some channels affecting it
    transformTagAnimator <- case maybeName of

      Just name -> do
        -- list of transform combinators for channels affecting transform
        channelAnimators <- fmap concat $ forM channels $ \ColladaChannelTag
          { cctTarget = target
          , cctSamplerElement = samplerElement
          } -> case T.stripPrefix (nodeId <> "/" <> name) target of
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

                _ -> throwError $ "unknown path for translate tag: " <> path

              ColladaRotateTag _initialAxis _initialAngle -> case path of

                "" -> do
                  a <- animateSampler samplerElement
                  return [\(ColladaRotateTag _axis _angle) time -> let
                    Vec4 x y z angle = a time
                    in ColladaRotateTag (Vec3 x y z) (angle * pi / 180)]

                ".ANGLE" -> do
                  a <- animateSampler samplerElement
                  return [\(ColladaRotateTag axis _angle) time -> ColladaRotateTag axis (a time * pi / 180)]

                _ -> throwError $ "unknown path for rotate tag: " <> path

              ColladaMatrixTag _initialMat -> case path of

                "" -> do
                  a <- animateSampler samplerElement
                  return [\(ColladaMatrixTag _matrix) time -> ColladaMatrixTag (unitMat `mul` (a time :: Float4x4) `mul` invUnitMat)]

                _ -> throwError $ "unknown path for matrix tag: " <> path

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

class Stridable s a where
  stridableStride :: s a -> Int
  constructStridable :: VG.Vector v a => v a -> Int -> s a

instance Vectorized a => Stridable Vec2 a where
  stridableStride _ = 2
  constructStridable v i = Vec2 (q 0) (q 1) where q j = v VG.! (i + j)

instance Vectorized a => Stridable Vec3 a where
  stridableStride _ = 3
  constructStridable v i = Vec3 (q 0) (q 1) (q 2) where q j = v VG.! (i + j)

instance Vectorized a => Stridable Vec4 a where
  stridableStride _ = 4
  constructStridable v i = Vec4 (q 0) (q 1) (q 2) (q 3) where q j = v VG.! (i + j)

instance Vectorized a => Stridable Mat4x4 a where
  stridableStride _ = 16
  constructStridable v i = Mat4x4
    (q 0) (q 1) (q 2) (q 3)
    (q 4) (q 5) (q 6) (q 7)
    (q 8) (q 9) (q 10) (q 11)
    (q 12) (q 13) (q 14) (q 15)
    where q j = v VG.! (i + j)

-- | Convert vector of primitive values to vector of Stridables.
stridableStream :: (Storable a, Storable (s a), Stridable s a) => VS.Vector a -> VS.Vector (s a)
stridableStream = f undefined where
  f :: (Storable a, Storable (s a), Stridable s a) => s a -> VS.Vector a -> VS.Vector (s a)
  f u v = VS.generate (VG.length v `quot` stride) $ \i -> constructStridable v $ i * stride where
    stride = stridableStride u

parseStridables :: (Storable a, Storable (s a), Stridable s a) => (VS.Vector a, Int) -> ColladaM (VS.Vector (s a))
parseStridables (q, stride) = f undefined q where
  f :: (Storable a, Storable (s a), Stridable s a) => s a -> VS.Vector a -> ColladaM (VS.Vector (s a))
  f u v = if stride == stridableStride u
    then return $ stridableStream v
    else throwError "wrong stride"

class Animatable a where
  animatableStride :: a -> Int
  animatableConstructor :: VS.Vector Float -> Int -> a
  interpolateAnimatable :: Float -> a -> a -> a

instance Animatable Float where
  animatableStride _ = 1
  animatableConstructor v i = v VG.! i
  interpolateAnimatable t a b = a * (1 - t) + b * t

instance Animatable Float3 where
  animatableStride = stridableStride
  animatableConstructor v i = Vec3 (v VG.! i) (v VG.! (i + 1)) (v VG.! (i + 2))
  interpolateAnimatable t a b = a * vecFromScalar (1 - t) + b * vecFromScalar t

instance Animatable Float4 where
  animatableStride = stridableStride
  animatableConstructor v i = Vec4 (v VG.! i) (v VG.! (i + 1)) (v VG.! (i + 2)) (v VG.! (i + 3))
  interpolateAnimatable t a b = a * vecFromScalar (1 - t) + b * vecFromScalar t

instance Animatable Float4x4 where
  animatableStride = stridableStride
  animatableConstructor = constructStridable
  interpolateAnimatable t a b = a * matFromScalar (1 - t) + b * matFromScalar t

animateSampler :: Animatable a => XML.Element -> ColladaM (Float -> a)
animateSampler element = do
  ((inputs, 1), (outputs, outputStride)) <- parseSampler element
  let
    len = VG.length inputs
    search time left right = if left + 1 < right then let
      mid = (left + right) `quot` 2
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
  , csklnParentId :: {-# UNPACK #-} !Int
  } deriving Show

-- | Create flat skeleton structure for node hierarchy.
parseSkeleton :: XML.Element -> ColladaM ColladaSkeleton
parseSkeleton element = do
  rootNodeTag <- parseNode element
  let
    go parentId currentId nodeTag@ColladaNodeTag
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
    (_, nodes) = go (-1) 0 rootNodeTag
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
      let
        ColladaSkeletonNode
          { csklnParentId = parentId
          } = nodes V.! i
      parentTransform <- if parentId >= 0 then VM.read transforms parentId else return rootTransform
      VM.write transforms i $ combineTransform parentTransform $ (nodeAnimators V.! i) time
    return transforms

newtype ColladaSkin t = ColladaSkin
  {
  -- | Bones used in skinning, in order corresponding to bone indices in mesh.
    cskinBones :: V.Vector (ColladaBone t)
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
  ColladaSettings
    { csUnitMat = unitMat
    , csInvUnitMat = invUnitMat
    } <- fmap ccSettings get

  bindShapeTransform <- fmap (\v -> constructStridable v 0 :: Float4x4) (parseArray =<< getSingleChildWithTag "bind_shape_matrix" skinElement)

  jointsElement <- getSingleChildWithTag "joints" skinElement
  jointsInputs <- mapM parseInput =<< getChildrenWithTag "input" jointsElement

  let
    findInput inputs semantic parent = case filter (\input -> citSemantic input == semantic) inputs of
      [input] -> return input :: ColladaM ColladaInputTag
      _ -> throwError $ "no single " <> parent <> " input with " <> semantic <> " semantic"

  jointsJointInput <- findInput jointsInputs "JOINT" "joints"
  jointsJointNames <- fmap fst $ parseSource $ citSourceElement jointsJointInput
  jointsInvBindMatrixInput <- findInput jointsInputs "INV_BIND_MATRIX" "joints"
  jointsInvBindTransforms <- parseStridables =<< parseSource (citSourceElement jointsInvBindMatrixInput)

  skinBones <- flip VG.imapM jointsJointNames $ \i jointName -> case V.findIndex (\ColladaSkeletonNode
    { csklnNodeTag = ColladaNodeTag
      { cntSID = sid
      }
    } -> sid == jointName) nodes of
    Just nodeIndex -> return ColladaBone
      { cboneSkeletonIndex = nodeIndex
      , cboneInvBindTransform = transformFromMatrix $ unitMat `mul` (jointsInvBindTransforms VG.! i :: Float4x4) `mul` bindShapeTransform `mul` invUnitMat
      }
    Nothing -> throwError $ "missing skeleton node for joint " <> jointName

  let
    namedBones = MS.fromList $ zip (VG.toList jointsJointNames) [0..]

  vertexWeightsElement <- getSingleChildWithTag "vertex_weights" skinElement
  vertexWeightsInputs <- mapM parseInput =<< getChildrenWithTag "input" vertexWeightsElement
  let
    vertexWeightsStride = length vertexWeightsInputs

  vertexWeightsJointInput <- findInput vertexWeightsInputs "JOINT" "vertex_weights"
  vertexWeightsJointNames <- fmap fst $ parseSource $ citSourceElement vertexWeightsJointInput
  let
    vertexWeightsJointOffset = citOffset vertexWeightsJointInput
  vertexWeightsJointBones <- VG.forM vertexWeightsJointNames $ \jointName -> case MS.lookup jointName namedBones of
    Just bone -> return bone
    Nothing -> throwError $ "missing bone for joint " <> jointName

  vertexWeightsWeightInput <- findInput vertexWeightsInputs "WEIGHT" "vertex_weights"
  vertexWeightsWeights <- fmap fst $ parseSource $ citSourceElement vertexWeightsWeightInput
  let
    vertexWeightsWeightOffset = citOffset vertexWeightsWeightInput

  count <- parse <$> getElementAttr "count" vertexWeightsElement
  vcount <- parseArray =<< getSingleChildWithTag "vcount" vertexWeightsElement
  v <- parseArray =<< getSingleChildWithTag "v" vertexWeightsElement

  let
    -- constant
    bonesPerVertex = 4

    (rawWeights, rawBones) = unsafePerformIO $ do
      weights <- VSM.new $ count * bonesPerVertex
      bones <- VSM.new $ count * bonesPerVertex
      -- loop for vertices
      let
        f j i bonesCount = do
          weightJointPairs <- VUM.new $ fromIntegral (bonesCount :: Word32)

          -- loop for bones of vertex
          forM_ [0..(fromIntegral bonesCount - 1)] $ \k -> let
            o = (j + k) * vertexWeightsStride
            jointIndex = v VG.! (o + vertexWeightsJointOffset) :: Word32
            weightIndex = v VG.! (o + vertexWeightsWeightOffset)
            in VGM.write weightJointPairs k (vertexWeightsWeights VG.! fromIntegral weightIndex, vertexWeightsJointBones VG.! fromIntegral jointIndex)

          -- sort weight-joint pairs
          VAI.sort weightJointPairs

          -- pick up most weighted
          freezedWeightJointPairs <- VU.unsafeFreeze weightJointPairs
          let
            len = VG.length freezedWeightJointPairs
            bestWeightJointPairs =
              if len >= bonesPerVertex then VG.drop (len - bonesPerVertex) freezedWeightJointPairs
              else freezedWeightJointPairs VG.++ VG.fromList (replicate (bonesPerVertex - len) (0, 0))

            -- calc sum of weights to normalize
            weightSum = VG.sum $ VG.map fst bestWeightJointPairs

          -- write weights and bones
          forM_ [0..(bonesPerVertex - 1)] $ \k -> do
            let
              (weight, bone) = bestWeightJointPairs VG.! k
            VGM.write weights (i * bonesPerVertex + k) $ weight / weightSum
            VGM.write bones (i * bonesPerVertex + k) bone

          return $ j + fromIntegral bonesCount
        in VG.ifoldM'_ f 0 vcount

      freezedWeights <- VG.unsafeFreeze weights
      freezedBones <- VG.unsafeFreeze bones
      return (stridableStream freezedWeights, stridableStream freezedBones)

  verticesData <- parseGeometry =<< resolveElement =<< getElementAttr "source" skinElement

  positionIndices <- cvdPositionIndices verticesData

  return (verticesData
    { cvdWeights = return $ VS.generate (VG.length positionIndices) $ \i -> rawWeights VG.! fromIntegral (positionIndices VG.! i)
    , cvdBones = return $ VS.generate (VG.length positionIndices) $ \i -> rawBones VG.! fromIntegral (positionIndices VG.! i)
    }, ColladaSkin
    { cskinBones = skinBones
    })

-- Re-export of some XML types.
type ColladaElement = XML.Element
