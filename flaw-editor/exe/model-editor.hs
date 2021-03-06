{-|
Module: Main
Description: Entry point for flaw model-editor executable.
License: MIT
-}

{-# LANGUAGE DeriveGeneric, OverloadedStrings, PatternSynonyms, RankNTypes, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

module Main(main) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Default
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import GHC.Generics(Generic)
import System.IO.Unsafe

import Flaw.App
import Flaw.Asset.Collada
import Flaw.Asset.Texture.Dxt
import Flaw.Book
import Flaw.Editor.UI.Elements
import Flaw.Editor.UI.FileDialog
import Flaw.Flow
import Flaw.Graphics
import Flaw.Graphics.Program
import Flaw.Graphics.Sampler
import Flaw.Graphics.Texture
import Flaw.Input.Keyboard
import Flaw.Input.Mouse
import Flaw.Math
import Flaw.Math.Geometry
import Flaw.UI
import Flaw.UI.CheckBox
import Flaw.UI.DefaultStyle
import Flaw.UI.Drawer
import Flaw.UI.EditBox
import Flaw.UI.Layout
import Flaw.UI.Panel
import Flaw.UI.RenderBox
import Flaw.UI.Slider
import Flaw.UI.Window
import Flaw.Visual
import Flaw.Visual.Frustum
import Flaw.Visual.Geometry
import Flaw.Visual.Geometry.Basic
import Flaw.Visual.Geometry.Vertex
import Flaw.Visual.Pipeline
import Flaw.Visual.Pipeline.Deferred
import Flaw.Visual.Pipeline.Shadow
import Flaw.Visual.ScreenQuad
import Flaw.Visual.Texture
import Flaw.Visual.Texture.Mip
import Flaw.Window

data Pipeline d = Pipeline
  { pipelineDeferred :: !(DeferredPipeline d)
  , pipelineShadow :: !(ShadowPipeline d)
  , pipelineShadowBlur :: !(ShadowBlurPipeline d)
  }

data TextureCell d = TextureCell
  { textureCellBook :: !Book
  , textureCellTexture :: !(TextureId d)
  , textureCellFileName :: !T.Text
  }

instance Device d => S.Serialize (TextureCell d) where
  put = S.put . textureCellFileName
  get = do
    fileName <- S.get
    return TextureCell
      { textureCellBook = dummyBook
      , textureCellTexture = nullTexture
      , textureCellFileName = fileName
      }

data GeometryCell d = GeometryCell
  { geometryCellGeometry :: !(Geometry d)
  , geometryCellFileName :: !T.Text
  , geometryCellNodeName :: !T.Text
  }

instance Device d => S.Serialize (GeometryCell d) where
  put GeometryCell
    { geometryCellFileName = fileName
    , geometryCellNodeName = nodeName
    } = do
    S.put fileName
    S.put nodeName
  get = do
    fileName <- S.get
    nodeName <- S.get
    return GeometryCell
      { geometryCellGeometry = Geometry
        { geometryVertexBuffer = nullVertexBuffer
        , geometryIndexBuffer = nullIndexBuffer
        , geometryIndicesCount = 0
        }
      , geometryCellFileName = fileName
      , geometryCellNodeName = nodeName
      }

{-# NOINLINE dummyBook #-}
dummyBook :: Book
dummyBook = unsafePerformIO newBook

newTextureCell :: IO (TextureId d, IO ()) -> IO (TextureCell d, IO ())
newTextureCell createTexture = withSpecialBook $ \bk -> do
  cellBook <- book bk newDynamicBook
  cellTexture <- book cellBook createTexture
  return TextureCell
    { textureCellBook = cellBook
    , textureCellTexture = cellTexture
    , textureCellFileName = T.empty
    }

data EditorState d = EditorState
  { editorStateEyeSpeed :: !Float3
  , editorStateEyeAlpha :: !Float
  , editorStateEyeBeta :: !Float
  , editorStateEyeFrustum :: !Frustum
  , editorStateMaterial :: !Float4
  , editorStateLightFrustum :: !Frustum
  , editorStateLightColor :: !Float3
  , editorStateAmbientLightColor :: !Float3
  , editorStateGeometryCell :: !(GeometryCell d)
  , editorStateAlbedoTextureCell :: !(TextureCell d)
  , editorStateNormalTextureCell :: !(TextureCell d)
  , editorStateNormalTextureEnabled :: !Bool
  , editorStateDiffuseTextureCell :: !(TextureCell d)
  , editorStateDiffuseTextureEnabled :: !Bool
  , editorStateSpecularTextureCell :: !(TextureCell d)
  , editorStateSpecularTextureEnabled :: !Bool
  , editorStateMetalnessTextureCell :: !(TextureCell d)
  , editorStateMetalnessTextureEnabled :: !Bool
  , editorStateGlossinessTextureCell :: !(TextureCell d)
  , editorStateGlossinessTextureEnabled :: !Bool
  , editorStateEmissionTextureCell :: !(TextureCell d)
  , editorStateEmissionTextureEnabled :: !Bool
  , editorStateLinearFiltering :: !Bool
  } deriving Generic

instance Device d => S.Serialize (EditorState d)

deriving instance Generic Frustum
instance S.Serialize Frustum

pattern SHADER_FLAG_NORMAL_MAP = 1
pattern SHADER_FLAG_DIFFUSE_MAP = 2
pattern SHADER_FLAG_SPECULAR_MAP = 4
pattern SHADER_FLAG_METALNESS_MAP = 8
pattern SHADER_FLAG_GLOSSINESS_MAP = 16
pattern SHADER_FLAG_EMISSION_MAP = 32
pattern SHADER_FLAGS_COUNT = 64

pattern SHADOW_MAP_SIZE = 1024

getEyeDirection :: EditorState d -> Float3
getEyeDirection EditorState
  { editorStateEyeAlpha = eyeAlpha
  , editorStateEyeBeta = eyeBeta
  } = Float3 (ca * cb) (sa * cb) sb where
  ca = cos eyeAlpha
  sa = sin eyeAlpha
  cb = cos eyeBeta
  sb = sin eyeBeta

main :: IO ()
main = withApp def
  { appConfigTitle = "FLAW Model Editor"
  , appConfigNeedDepthBuffer = False
  } $ \window device context presenter inputManager -> withBook $ \bk -> handle (errorHandler "main") $ do

  -- UI styles and drawer
  let metrics = defaultStyleMetrics
  drawer <- book bk $ initDefaultStyleDrawer device

  exitVar <- newTVarIO False

  -- create sphere geometry
  sphereGeometry <- book bk $ let
    f alpha beta = let
      p = Vec3 (cos alpha * cos beta) (sin alpha * cos beta) (sin beta)
      in VertexPNT
        { f_VertexPNT_position = p
        , f_VertexPNT_normal = p
        , f_VertexPNT_texcoord = Vec2 (alpha / (pi * 2)) (beta / pi + 0.5)
        }
    in loadPackedGeometry device $ packGeometry (sphereVertices f 16 16 :: VS.Vector VertexPNT)

  let configFileName = "config.bin"

  -- template editor state
  configData <- handle (\SomeException {} -> return B.empty) $ B.readFile configFileName
  let
    nullTextureCell = TextureCell
      { textureCellTexture = nullTexture
      , textureCellBook = dummyBook
      , textureCellFileName = T.empty
      }
    templateEditorState = case S.decode configData of
      Left _ -> EditorState
        { editorStateEyeSpeed = Vec3 0 0 0
        , editorStateEyeAlpha = 0
        , editorStateEyeBeta = 0
        , editorStateEyeFrustum = lookAtFrustum (Vec3 (-5) 0 0) (Vec3 0 0 0) (Vec3 0 0 1) identityFrustum
        , editorStateMaterial = Vec4 0.2 0.5 0 0.4
        , editorStateLightFrustum = lookAtFrustum (Vec3 0 2 0) (Vec3 0 0 0) (Vec3 0 0 1) identityFrustum
        , editorStateLightColor = vecFromScalar 10
        , editorStateAmbientLightColor = vecFromScalar 0.01
        , editorStateGeometryCell = GeometryCell
          { geometryCellGeometry = sphereGeometry
          , geometryCellFileName = T.empty
          , geometryCellNodeName = T.empty
          }
        , editorStateAlbedoTextureCell = nullTextureCell
        , editorStateNormalTextureCell = nullTextureCell
        , editorStateNormalTextureEnabled = False
        , editorStateDiffuseTextureCell = nullTextureCell
        , editorStateDiffuseTextureEnabled = False
        , editorStateSpecularTextureCell = nullTextureCell
        , editorStateSpecularTextureEnabled = False
        , editorStateMetalnessTextureCell = nullTextureCell
        , editorStateMetalnessTextureEnabled = False
        , editorStateGlossinessTextureCell = nullTextureCell
        , editorStateGlossinessTextureEnabled = False
        , editorStateEmissionTextureCell = nullTextureCell
        , editorStateEmissionTextureEnabled = False
        , editorStateLinearFiltering = False
        }
      Right es -> es

  -- editor state
  editorStateVar <- do
    initialAlbedoTextureCell <- book bk $ newTextureCell $ do
      let
        width = 256
        height = 256
        step = 16
      createStaticTexture device TextureInfo
        { textureWidth = width
        , textureHeight = height
        , textureDepth = 0
        , textureMips = 1
        , textureFormat = UncompressedTextureFormat
          { textureFormatComponents = PixelRGBA
          , textureFormatValueType = PixelUint
          , textureFormatPixelSize = Pixel32bit
          , textureFormatColorSpace = StandardColorSpace
          }
        , textureCount = 0
        } def $ B.pack $ do
        i <- [0..(height - 1)]
        j <- [0..(width - 1)]
        let c = if even (i `quot` step + j `quot` step) then 255 else 0
        [c, c, c, 255]
    initialNormalTextureCell <- book bk $ newTextureCell $ let
      width = 1024
      height = 1024
      step = 64
      (compressedTextureInfo, compressedTextureBytes) = dxtCompressTexture TextureInfo
        { textureWidth = width
        , textureHeight = height
        , textureDepth = 0
        , textureMips = 1
        , textureFormat = UncompressedTextureFormat
          { textureFormatComponents = PixelRG
          , textureFormatValueType = PixelUint
          , textureFormatPixelSize = Pixel16bit
          , textureFormatColorSpace = LinearColorSpace
          }
        , textureCount = 0
        } $ B.pack $ do
        i <- [0..(height - 1)]
        j <- [0..(width - 1)]
        let
          ii = i `rem` step
          jj = j `rem` step
          Float3 x y _z = normalize $
            if ii < jj then
              if step - ii < jj then Vec3 1 0 1
              else Vec3 0 (-1) 1
            else
              if step - ii < jj then Vec3 0 1 1
              else Vec3 (-1) 0 1
        [fromIntegral (floor (x * 127) + 127 :: Int), fromIntegral (floor (y * 127) + 127 :: Int)]
      in createStaticTexture device compressedTextureInfo def compressedTextureBytes
    let
      newGrayTextureCell = book bk $ newTextureCell $ let
        width = 4
        height = 4
        textureInfo = TextureInfo
          { textureWidth = width
          , textureHeight = height
          , textureDepth = 0
          , textureMips = 1
          , textureFormat = UncompressedTextureFormat
            { textureFormatComponents = PixelRGBA
            , textureFormatValueType = PixelUint
            , textureFormatPixelSize = Pixel32bit
            , textureFormatColorSpace = StandardColorSpace
            }
          , textureCount = 0
          }
        textureBytes = B.pack $ concat $ replicate (width * height) [127, 127, 127, 255]
        in createStaticTexture device textureInfo def textureBytes
    initialDiffuseTextureCell <- newGrayTextureCell
    initialSpecularTextureCell <- newGrayTextureCell
    initialMetalnessTextureCell <- newGrayTextureCell
    initialGlossinessTextureCell <- newGrayTextureCell
    initialEmissionTextureCell <- newGrayTextureCell
    newTVarIO templateEditorState
      { editorStateGeometryCell = GeometryCell
        { geometryCellGeometry = sphereGeometry
        , geometryCellFileName = T.empty
        , geometryCellNodeName = T.empty
        }
      , editorStateAlbedoTextureCell = initialAlbedoTextureCell
      , editorStateNormalTextureCell = initialNormalTextureCell
      , editorStateDiffuseTextureCell = initialDiffuseTextureCell
      , editorStateSpecularTextureCell = initialSpecularTextureCell
      , editorStateMetalnessTextureCell = initialMetalnessTextureCell
      , editorStateGlossinessTextureCell = initialGlossinessTextureCell
      , editorStateEmissionTextureCell = initialEmissionTextureCell
      }

  -- render pipeline
  updatePipeline <- book bk $ newPipelineWrapper $ \width height -> withSpecialBook $ \pbk -> do
    deferredPipeline <- book pbk $ newDeferredPipeline device width height
    shadowPipeline <- book pbk $ newShadowPipeline device SHADOW_MAP_SIZE SHADOW_MAP_SIZE 1
    shadowBlurPipeline <- book pbk $ newShadowBlurPipeline device SHADOW_MAP_SIZE SHADOW_MAP_SIZE
    return Pipeline
      { pipelineDeferred = deferredPipeline
      , pipelineShadow = shadowPipeline
      , pipelineShadowBlur = shadowBlurPipeline
      }

  -- programs
  ubsEye <- uniformBufferSlot 0
  uEyeFrustum <- uniformFrustum ubsEye
  ubsLight <- uniformBufferSlot 1
  uLightFrustum <- uniformFrustum ubsLight
  uLightColor <- uniform ubsLight
  ubsObject <- uniformBufferSlot 2
  uWorld <- uniform ubsObject
  uMaterial <- uniform ubsObject

  usEye <- book bk $ createUniformStorage device ubsEye
  usLight <- book bk $ createUniformStorage device ubsLight
  usObject <- book bk $ createUniformStorage device ubsObject
  -- shadow program
  shadowProgram <- book bk $ createProgram device $ do
    let v = undefined :: VertexPNT
    aPosition <- vertexAttribute 0 0 $ vertexPositionAttribute v
    worldPosition <- temp $ mul uWorld $ cvec31 aPosition (constf 1)
    rasterize (frustumNodeViewProj uEyeFrustum `mul` worldPosition) $ colorTarget 0 0
  -- opaque programs
  opaquePrograms <- V.generateM SHADER_FLAGS_COUNT $ \shaderFlags -> book bk $ createProgram device $ do
    let v = undefined :: VertexPNT
    aPosition <- vertexAttribute 0 0 $ vertexPositionAttribute v
    aNormal <- vertexAttribute 0 0 $ vertexNormalAttribute v
    aTexcoord <- vertexAttribute 0 0 $ vertexTexcoordAttribute v
    worldPosition <- temp $ mul uWorld $ cvec31 aPosition (constf 1)
    viewPosition <- temp $ frustumNodeView uEyeFrustum `mul` worldPosition
    vertexViewNormal <- temp $ xyz__ $ mul (frustumNodeView uEyeFrustum) $ cvec31 aNormal (constf 0)
    rasterize (frustumNodeViewProj uEyeFrustum `mul` worldPosition) $ do
      albedo <- temp $ sample (sampler2D3f 0) aTexcoord
      resultViewNormal <- if (shaderFlags .&. SHADER_FLAG_NORMAL_MAP) > 0 then do
        (viewTangent, viewBinormal, viewNormal) <- tangentFrame (xyz__ viewPosition) vertexViewNormal aTexcoord
        bentNormalXY <- temp $ sample (sampler2D2f 1) aTexcoord * vecFromScalar (255 / 127) - vecFromScalar 1
        --bentNormalXY <- temp $ (sample (sampler2D2f 1) aTexcoord * vecFromScalar (255 / 127) - vecFromScalar 1) * cvec11 0 0
        --bentNormalXY <- temp $ (sample (sampler2D2f 1) aTexcoord * vecFromScalar (255 / 127) - vecFromScalar 1) * vecFromScalar 0 + (cvec11 (128 / 255) (127 / 255) * vecFromScalar (255 / 127) - vecFromScalar 1)
        bentNormal <- temp $ cvec21 bentNormalXY $ sqrt $ max_ 0 $ 1 - dot bentNormalXY bentNormalXY
        temp $ viewTangent * xxx__ bentNormal + viewBinormal * yyy__ bentNormal + viewNormal * zzz__ bentNormal
        else return vertexViewNormal
      let
        occlusion = 1
        diffuse    = if (shaderFlags .&. SHADER_FLAG_DIFFUSE_MAP   ) > 0 then sample (sampler2Df 2) aTexcoord else x_ uMaterial
        specular   = if (shaderFlags .&. SHADER_FLAG_SPECULAR_MAP  ) > 0 then sample (sampler2Df 3) aTexcoord else y_ uMaterial
        metalness  = if (shaderFlags .&. SHADER_FLAG_METALNESS_MAP ) > 0 then sample (sampler2Df 4) aTexcoord else z_ uMaterial
        glossiness = if (shaderFlags .&. SHADER_FLAG_GLOSSINESS_MAP) > 0 then sample (sampler2Df 5) aTexcoord else w_ uMaterial
        emission   = if (shaderFlags .&. SHADER_FLAG_EMISSION_MAP  ) > 0 then sample (sampler2D3f 6) aTexcoord else 0

      outputDeferredPipelineOpaquePass emission (cvec31 albedo occlusion) (cvec1111 diffuse specular metalness glossiness) resultViewNormal
  -- lightbulb opaque program
  lightbulbOpaqueProgram <- book bk $ createProgram device $ do
    let v = undefined :: VertexPNT
    aPosition <- vertexAttribute 0 0 $ vertexPositionAttribute v
    aNormal <- vertexAttribute 0 0 $ vertexNormalAttribute v
    worldPosition <- temp $ mul uWorld $ cvec31 aPosition (constf 1)
    vertexViewNormal <- temp $ xyz__ $ mul (frustumNodeView uEyeFrustum) $ cvec31 aNormal (constf 0)
    rasterize (frustumNodeViewProj uEyeFrustum `mul` worldPosition) $ do
      viewNormal <- temp $ normalize vertexViewNormal
      outputDeferredPipelineOpaquePass uLightColor (const4f $ Vec4 0 0 0 0) (const4f $ Vec4 0 0 0 0) viewNormal
  -- screen quad
  screenQuadRenderer <- book bk $ newScreenQuadRenderer device
  -- shadow blurer
  shadowBlurer <- book bk $ newShadowBlurerESM device 3 $ frustumDepthHomogeneousToLinear uEyeFrustum
  -- light program
  lightProgram <- book bk $ createProgram device $ deferredPipelineLightPassProgram (frustumNodeInvProj uEyeFrustum) $ \viewPosition -> do
    toLight <- temp $ frustumNodeEye uLightFrustum - viewPosition
    toLightDirection <- temp $ normalize toLight
    lightColor <- temp $ uLightColor / vecFromScalar (dot toLight toLight)
    shadow <- shadowBlurerESMInput (frustumNodeViewProj uLightFrustum) viewPosition 0 (frustumProjCoordToLinearDepth uLightFrustum)
    return (toLightDirection, lightColor * vecFromScalar shadow)
  -- ambient light program
  ambientLightProgram <- book bk $ createProgram device $ deferredPipelineAmbientLightPassProgram (frustumNodeInvProj uEyeFrustum) uLightColor
  -- tone mapping
  toneMappingProgram <- book bk $ createProgram device $ screenQuadProgram $ \screenPositionTexcoord -> do
    xyY <- xyz2xyY =<< rgb2xyz (sample (sampler2D3f 0) $ zw__ screenPositionTexcoord)
    rgb <- xyz2rgb =<< xyY2xyz (cvec111 (x_ xyY) (y_ xyY) (z_ xyY / (1 + z_ xyY)))
    colorTarget 0 $ cvec31 rgb 1

  linearSamplerState <- book bk $ createSamplerState device def
    { samplerMinFilter = SamplerLinearFilter
    , samplerMipFilter = SamplerLinearFilter
    , samplerMagFilter = SamplerLinearFilter
    }

  let
    cameraNearPlane = 0.01
    cameraFarPlane = 10

  -- flow for state and rendering
  stateFlow <- book bk newFlow
  -- flow for background operations
  backgroundFlow <- book bk newFlow
  -- flow for heavy processing operations
  processingFlow <- book bk newFlow

  let
    loadColladaFile fileName elementId = handle (errorHandler "loadColladaFile") $ do
      bytes <- BL.readFile $ T.unpack fileName
      let
        eitherVertices = runCollada $ do
          initColladaCache bytes
          createColladaVertices =<< parseGeometry =<< getElementById elementId
      case eitherVertices of
        Right vertices -> do
          geometry <- book bk $ loadPackedGeometry device $ packGeometry (vertices :: VS.Vector VertexPNT)
          runInFlow stateFlow $ atomically $ modifyTVar' editorStateVar $ \s@EditorState
            { editorStateGeometryCell = geometryCell
            } -> s
            { editorStateGeometryCell = geometryCell
              { geometryCellGeometry = geometry
              , geometryCellFileName = fileName
              , geometryCellNodeName = elementId
              }
            }
        Left err -> putStrLn $ T.unpack err

    loadTextureFile fileName isNormalTexture = do
      loadedTexture <- loadTexture <$> B.readFile (T.unpack fileName)
      let
        PackedTexture
          { packedTextureBytes = textureBytes
          , packedTextureInfo = textureInfo
          } = generateMips 0 $ if isNormalTexture then convertTextureToLinearRG loadedTexture else loadedTexture
        (compressedTextureInfo, compressedTextureBytes) = dxtCompressTexture textureInfo textureBytes
      createStaticTexture device compressedTextureInfo def compressedTextureBytes

  -- input callback
  let
    inputCallback inputEvent InputState
      { inputStateMouse = mouseInputState
      } = case inputEvent of
      KeyboardInputEvent keyboardEvent -> case keyboardEvent of
        KeyDownEvent KeyL -> do
          modifyTVar' editorStateVar $ \editorState@EditorState
            { editorStateEyeFrustum = eyeFrustum
            } -> editorState
            { editorStateLightFrustum = eyeFrustum
            }
          return True
        _ -> return False
      MouseInputEvent mouseEvent -> case mouseEvent of
        MouseDownEvent LeftMouseButton -> do
          asyncRunInFlow stateFlow $ setWindowMouseLock window True
          return True
        MouseUpEvent LeftMouseButton -> do
          asyncRunInFlow stateFlow $ setWindowMouseLock window False
          return True
        RawMouseMoveEvent dx dy _dz -> do
          buttonPressed <- getMouseButtonState mouseInputState LeftMouseButton
          when buttonPressed $ modifyTVar' editorStateVar $ \editorState@EditorState
            { editorStateEyeAlpha = alpha
            , editorStateEyeBeta = beta
            } -> editorState
            { editorStateEyeAlpha = alpha - dx * 0.001
            , editorStateEyeBeta = beta - dy * 0.001
            }
          return True
        _ -> return False
      _ -> return False

  -- init UI
  (mainWindow@Window
    { windowKeyboardState = keyboardState
    }, renderBox) <- atomically $ do
    windowPanel <- newPanel True
    mainWindow <- newWindow window inputManager windowPanel
    setWindowCloseHandler mainWindow $ writeTVar exitVar True

    fileDialogService <- newFileDialogService metrics windowPanel backgroundFlow

    initialEditorState <- readTVar editorStateVar

    -- properties frame
    colladaFileElement <- newFileElement fileDialogService
    colladaNodeEditBox <- newEditBox
    do
      let
        actionHandler = do
          fileName <- getText colladaFileElement
          nodeName <- getText colladaNodeEditBox
          asyncRunInFlow processingFlow $ loadColladaFile fileName nodeName
      setActionHandler colladaFileElement actionHandler
      -- load initial geometry
      let
        GeometryCell
          { geometryCellFileName = initialFileName
          , geometryCellNodeName = initialNodeName
          } = editorStateGeometryCell templateEditorState
      setText colladaFileElement initialFileName
      setText colladaNodeEditBox initialNodeName
      unless (T.null initialFileName || T.null initialNodeName) actionHandler

    let
      genericTextureFileElement isNormalTexture getTextureCell setTextureCell = do
        fileElement <- newFileElement fileDialogService
        let
          actionHandler = do
            fileName <- getText fileElement
            asyncRunInFlow processingFlow $ handle (errorHandler "genericTextureFileElement") $ do
              cell@TextureCell
                { textureCellBook = cellBook
                } <- getTextureCell <$> readTVarIO editorStateVar
              freePreviousTexture <- releaseBook cellBook
              texture <- book cellBook $ loadTextureFile fileName isNormalTexture
              runInFlow stateFlow $ atomically $ modifyTVar' editorStateVar $ setTextureCell cell
                { textureCellTexture = texture
                , textureCellFileName = fileName
                }
              freePreviousTexture
        setActionHandler fileElement actionHandler
        -- load initial texture
        let
          TextureCell
            { textureCellFileName = initialFileName
            } = getTextureCell templateEditorState
        setText fileElement initialFileName
        unless (T.null initialFileName) actionHandler
        return fileElement
    let
      textureFileElement = genericTextureFileElement False
      rgTextureFileElement = genericTextureFileElement True
    albedoTextureFileElement <- textureFileElement editorStateAlbedoTextureCell $ \c s -> s
      { editorStateAlbedoTextureCell = c
      }
    normalTextureFileElement <- rgTextureFileElement editorStateNormalTextureCell $ \c s -> s
      { editorStateNormalTextureCell = c
      }
    lightIntensitySlider <- newSlider metrics 0.01
    setFloatValue lightIntensitySlider $ 0.1 * x_ (editorStateLightColor initialEditorState)
    setChangeHandler lightIntensitySlider $ do
      value <- getFloatValue lightIntensitySlider
      modifyTVar' editorStateVar $ \s -> s
        { editorStateLightColor = vecFromScalar $ value * 10
        }
    ambientLightIntensitySlider <- newSlider metrics 0.01
    setFloatValue ambientLightIntensitySlider $ 10 * x_ (editorStateAmbientLightColor initialEditorState)
    setChangeHandler ambientLightIntensitySlider $ do
      value <- getFloatValue ambientLightIntensitySlider
      modifyTVar' editorStateVar $ \s -> s
        { editorStateAmbientLightColor = vecFromScalar $ value * 0.1
        }
    let initialMaterial = editorStateMaterial initialEditorState
    diffuseSlider <- newSlider metrics 0.01
    setFloatValue diffuseSlider $ x_ initialMaterial
    setChangeHandler diffuseSlider $ do
      value <- getFloatValue diffuseSlider
      modifyTVar' editorStateVar $ \s -> s
        { editorStateMaterial = editorStateMaterial s * Vec4 0 1 1 1 + Vec4 value 0 0 0
        }
    diffuseTextureFileElement <- textureFileElement editorStateDiffuseTextureCell $ \c s -> s
      { editorStateDiffuseTextureCell = c
      }
    specularSlider <- newSlider metrics 0.01
    setFloatValue specularSlider $ y_ initialMaterial
    setChangeHandler specularSlider $ do
      value <- getFloatValue specularSlider
      modifyTVar' editorStateVar $ \s -> s
        { editorStateMaterial = editorStateMaterial s * Vec4 1 0 1 1 + Vec4 0 value 0 0
        }
    specularTextureFileElement <- textureFileElement editorStateSpecularTextureCell $ \c s -> s
      { editorStateSpecularTextureCell = c
      }
    metalnessSlider <- newSlider metrics 0.01
    setFloatValue metalnessSlider $ z_ initialMaterial
    setChangeHandler metalnessSlider $ do
      value <- getFloatValue metalnessSlider
      modifyTVar' editorStateVar $ \s -> s
        { editorStateMaterial = editorStateMaterial s * Vec4 1 1 0 1 + Vec4 0 0 value 0
        }
    metalnessTextureFileElement <- textureFileElement editorStateMetalnessTextureCell $ \c s -> s
      { editorStateMetalnessTextureCell = c
      }
    glossinessSlider <- newSlider metrics 0.01
    setFloatValue glossinessSlider $ w_ initialMaterial
    setChangeHandler glossinessSlider $ do
      value <- getFloatValue glossinessSlider
      modifyTVar' editorStateVar $ \s -> s
        { editorStateMaterial = editorStateMaterial s * Vec4 1 1 1 0 + Vec4 0 0 0 value
        }
    glossinessTextureFileElement <- textureFileElement editorStateGlossinessTextureCell $ \c s -> s
      { editorStateGlossinessTextureCell = c
      }
    emissionTextureFileElement <- textureFileElement editorStateEmissionTextureCell $ \c s -> s
      { editorStateEmissionTextureCell = c
      }
    linearFilteringCheckBox <- newLabeledCheckBox "enable"
    setChecked linearFilteringCheckBox $ editorStateLinearFiltering initialEditorState
    setChangeHandler linearFilteringCheckBox $ do
      checked <- getChecked linearFilteringCheckBox
      modifyTVar' editorStateVar $ \s -> s
        { editorStateLinearFiltering = checked
        }
    propertiesFrame <- frameFlowLayout metrics $ do
      labeledFlowLayout "collada file" $ elementInFlowLayout colladaFileElement
      labeledFlowLayout "collada node" $ elementInFlowLayout colladaNodeEditBox
      labeledFlowLayout "albedo texture" $ elementInFlowLayout albedoTextureFileElement
      checkBoxedFlowLayout "normal texture" $ \checkBox -> do
        elementInFlowLayout normalTextureFileElement
        lift $ setChecked checkBox $ editorStateNormalTextureEnabled initialEditorState
        lift $ setChangeHandler checkBox $ do
          checked <- getChecked checkBox
          modifyTVar' editorStateVar $ \s -> s
            { editorStateNormalTextureEnabled = checked
            }
      labeledFlowLayout "light" $ elementInFlowLayout lightIntensitySlider
      labeledFlowLayout "ambient light" $ elementInFlowLayout ambientLightIntensitySlider
      labeledFlowLayout "diffuse" $ elementInFlowLayout diffuseSlider
      checkBoxedFlowLayout "diffuse texture" $ \checkBox -> do
        elementInFlowLayout diffuseTextureFileElement
        lift $ setChecked checkBox $ editorStateDiffuseTextureEnabled initialEditorState
        lift $ setChangeHandler checkBox $ do
          checked <- getChecked checkBox
          modifyTVar' editorStateVar $ \s -> s
            { editorStateDiffuseTextureEnabled = checked
            }
      labeledFlowLayout "specular" $ elementInFlowLayout specularSlider
      checkBoxedFlowLayout "specular texture" $ \checkBox -> do
        elementInFlowLayout specularTextureFileElement
        lift $ setChecked checkBox $ editorStateSpecularTextureEnabled initialEditorState
        lift $ setChangeHandler checkBox $ do
          checked <- getChecked checkBox
          modifyTVar' editorStateVar $ \s -> s
            { editorStateSpecularTextureEnabled = checked
            }
      labeledFlowLayout "metalness" $ elementInFlowLayout metalnessSlider
      checkBoxedFlowLayout "metalness texture" $ \checkBox -> do
        elementInFlowLayout metalnessTextureFileElement
        lift $ setChecked checkBox $ editorStateMetalnessTextureEnabled initialEditorState
        lift $ setChangeHandler checkBox $ do
          checked <- getChecked checkBox
          modifyTVar' editorStateVar $ \s -> s
            { editorStateMetalnessTextureEnabled = checked
            }
      labeledFlowLayout "glossiness" $ elementInFlowLayout glossinessSlider
      checkBoxedFlowLayout "glossiness texture" $ \checkBox -> do
        elementInFlowLayout glossinessTextureFileElement
        lift $ setChecked checkBox $ editorStateGlossinessTextureEnabled initialEditorState
        lift $ setChangeHandler checkBox $ do
          checked <- getChecked checkBox
          modifyTVar' editorStateVar $ \s -> s
            { editorStateGlossinessTextureEnabled = checked
            }
      checkBoxedFlowLayout "emission texture" $ \checkBox -> do
        elementInFlowLayout emissionTextureFileElement
        lift $ setChecked checkBox $ editorStateEmissionTextureEnabled initialEditorState
        lift $ setChangeHandler checkBox $ do
          checked <- getChecked checkBox
          modifyTVar' editorStateVar $ \s -> s
            { editorStateEmissionTextureEnabled = checked
            }
      labeledFlowLayout "linear filtering" $ elementInFlowLayout linearFilteringCheckBox
    setText propertiesFrame "properties"
    propertiesFrameChild <- addFreeChild windowPanel propertiesFrame
    setSelfFreeChild propertiesFrame windowPanel propertiesFrameChild True

    -- render box
    renderBox <- newRenderBox (\_ _ -> return $ return ()) inputCallback
    renderBoxChild <- addFreeChild windowPanel renderBox

    -- window panel layout
    setLayoutHandler windowPanel $ \size -> do
      placeFreeChild windowPanel renderBoxChild $ Vec2 0 0
      layoutElement renderBox size

    return (mainWindow, renderBox)

  -- run
  runApp $ \frameTime -> do
    -- process input and window events
    processWindow mainWindow

    -- process camera
    liftIO $ atomically $ do
      focused <- isFocused renderBox
      when focused $ do
        editorState@EditorState
          { editorStateEyeFrustum = eyeFrustum@Frustum
            { frustumEye = position
            }
          , editorStateEyeSpeed = speed
          } <- readTVar editorStateVar
        w <- getKeyState keyboardState KeyW
        s <- getKeyState keyboardState KeyS
        a <- getKeyState keyboardState KeyA
        d <- getKeyState keyboardState KeyD
        q <- getKeyState keyboardState KeyQ
        e <- getKeyState keyboardState KeyE
        up <- getKeyState keyboardState KeyUp
        down <- getKeyState keyboardState KeyDown
        left <- getKeyState keyboardState KeyLeft
        right <- getKeyState keyboardState KeyRight
        let
          acceleration = 5
          direction = getEyeDirection editorState
          forward = direction
          rightward = normalize $ cross direction (Float3 0 0 1)
          upward = normalize $ cross rightward forward
          newSpeed = (speed + acceleration *
            ( forward * vecFromScalar (frameTime * ((if w || up then 1 else 0) + (if s || down then (-1) else 0)))
            + rightward * vecFromScalar (frameTime * ((if d || right then 1 else 0) + (if a || left then (-1) else 0)))
            + upward * vecFromScalar (frameTime * ((if e then 1 else 0) + (if q then (-1) else 0)))
            )) * vecFromScalar (exp (log 0.01 * frameTime))
          newSpeedNorm = norm newSpeed
          maxSpeed = 10
          newSpeed2 = if newSpeedNorm > maxSpeed then newSpeed * vecFromScalar (maxSpeed / newSpeedNorm) else newSpeed
        writeTVar editorStateVar editorState
          { editorStateEyeFrustum = eyeFrustum
            { frustumEye = position + speed * vecFromScalar frameTime
            }
          , editorStateEyeSpeed = newSpeed2
          }

    -- update drawer
    atomically $ setDrawerFrameTime drawer frameTime

    -- render window
    windowRender <- atomically $ renderWindow mainWindow drawer

    -- run render in state flow, so setting resources is synchronized
    runInFlow stateFlow $ render context $ present presenter $ do

      editorState@EditorState
        { editorStateEyeFrustum = Frustum
          { frustumEye = eyePosition
          }
        , editorStateMaterial = material
        , editorStateLightFrustum = lightFrustum
        , editorStateLightColor = lightColor
        , editorStateAmbientLightColor = ambientLightColor
        , editorStateGeometryCell = GeometryCell
          { geometryCellGeometry = Geometry
            { geometryVertexBuffer = vbObject
            , geometryIndexBuffer = ibObject
            , geometryIndicesCount = icObject
            }
          }
        , editorStateAlbedoTextureCell = TextureCell
          { textureCellTexture = tAlbedo
          }
        , editorStateNormalTextureCell = TextureCell
          { textureCellTexture = tNormal
          }
        , editorStateNormalTextureEnabled = normalTextureEnabled
        , editorStateDiffuseTextureCell = TextureCell
          { textureCellTexture = tDiffuse
          }
        , editorStateDiffuseTextureEnabled = diffuseTextureEnabled
        , editorStateSpecularTextureCell = TextureCell
          { textureCellTexture = tSpecular
          }
        , editorStateSpecularTextureEnabled = specularTextureEnabled
        , editorStateMetalnessTextureCell = TextureCell
          { textureCellTexture = tMetalness
          }
        , editorStateMetalnessTextureEnabled = metalnessTextureEnabled
        , editorStateGlossinessTextureCell = TextureCell
          { textureCellTexture = tGlossiness
          }
        , editorStateGlossinessTextureEnabled = glossinessTextureEnabled
        , editorStateEmissionTextureCell = TextureCell
          { textureCellTexture = tEmission
          }
        , editorStateEmissionTextureEnabled = emissionTextureEnabled
        , editorStateLinearFiltering = linearFiltering
        } <- liftIO $ readTVarIO editorStateVar

      Pipeline
        { pipelineDeferred = deferredPipeline@DeferredPipeline
          { deferredPipelineColorRTT = colorRTT
          }
        , pipelineShadow = shadowPipeline
        , pipelineShadowBlur = shadowBlurPipeline@ShadowBlurPipeline
          { shadowBlurPipelineRTT2 = shadowRTT
          }
        } <- updatePipeline

      -- setup camera
      let eyeDirection = getEyeDirection editorState
      Vec4 viewportLeft viewportTop viewportRight viewportBottom <- renderGetViewport
      let
        viewportWidth = viewportRight - viewportLeft
        viewportHeight = viewportBottom - viewportTop
        aspect = fromIntegral viewportWidth / fromIntegral viewportHeight

        eyeFrustum = lookAtFrustum eyePosition (eyePosition + eyeDirection) (Vec3 0 0 1) $ perspectiveFrustum (pi / 4) aspect cameraNearPlane cameraFarPlane identityFrustum

      liftIO $ atomically $ writeTVar editorStateVar $ editorState
        { editorStateEyeFrustum = eyeFrustum
        }

      -- light-space passes
      renderScope $ do
        renderUniformFrustum usEye uEyeFrustum lightFrustum
        renderUploadUniformStorage usEye
        renderUniformStorage usEye

        -- shadow pass
        renderScope $ do
          renderShadowPipelineShadowPass shadowPipeline
          renderClearDepth 0
          renderDepthTestFunc DepthTestFuncGreater

          renderProgram shadowProgram
          renderUniformStorage usObject
          renderVertexBuffer 0 vbObject
          renderIndexBuffer ibObject
          renderUniform usObject uWorld (affineIdentity :: Float4x4)
          renderUploadUniformStorage usObject
          renderUniformStorage usObject
          renderDraw icObject

        -- shadow blur passes
        renderShadowBlurerESM shadowPipeline shadowBlurPipeline shadowBlurer screenQuadRenderer

      -- camera-space passes
      renderScope $ do
        renderUniformFrustum usEye uEyeFrustum eyeFrustum
        renderUploadUniformStorage usEye
        renderUniformStorage usEye

        -- opaque pass
        renderScope $ do
          renderDeferredPipelineOpaquePass deferredPipeline

          renderClearColor 0 $ Float4 0 0 0 0
          renderClearColor 1 $ Float4 0 0 0 0
          renderClearColor 2 $ Float4 0 0 0 0
          renderClearColor 3 $ Float4 0 0 0 0
          renderClearDepth 0
          renderDepthTestFunc DepthTestFuncGreater

          renderProgram $ opaquePrograms V.!
            (   (if normalTextureEnabled then SHADER_FLAG_NORMAL_MAP else 0)
            .|. (if diffuseTextureEnabled then SHADER_FLAG_DIFFUSE_MAP else 0)
            .|. (if specularTextureEnabled then SHADER_FLAG_SPECULAR_MAP else 0)
            .|. (if metalnessTextureEnabled then SHADER_FLAG_METALNESS_MAP else 0)
            .|. (if glossinessTextureEnabled then SHADER_FLAG_GLOSSINESS_MAP else 0)
            .|. (if emissionTextureEnabled then SHADER_FLAG_EMISSION_MAP else 0)
            )
          renderUniformStorage usObject
          renderVertexBuffer 0 vbObject
          renderIndexBuffer ibObject
          let ss = if linearFiltering then linearSamplerState else nullSamplerState
          renderSampler 0 tAlbedo ss
          renderSampler 1 tNormal ss
          renderSampler 2 tDiffuse ss
          renderSampler 3 tSpecular ss
          renderSampler 4 tMetalness ss
          renderSampler 5 tGlossiness ss
          renderSampler 6 tEmission ss
          renderUniform usObject uWorld (affineIdentity :: Float4x4)
          renderUniform usObject uMaterial material
          renderUploadUniformStorage usObject
          renderUniformStorage usObject
          renderDraw icObject

          -- render lightbulb
          do
            let
              Geometry
                { geometryVertexBuffer = vb
                , geometryIndexBuffer = ib
                , geometryIndicesCount = ic
                } = sphereGeometry
            renderProgram lightbulbOpaqueProgram
            renderVertexBuffer 0 vb
            renderIndexBuffer ib
            renderUniform usObject uWorld $ affineTranslation (frustumEye lightFrustum) `mul` affineScaling (vecFromScalar 0.05 :: Float3)
            renderUploadUniformStorage usObject
            renderUniformStorage usObject
            renderUniform usLight uLightColor lightColor
            renderUploadUniformStorage usLight
            renderUniformStorage usLight
            renderDraw ic

        -- lighting pass
        renderScope $ do
          renderDeferredPipelineLightPass deferredPipeline

          renderUniformFrustum usLight uLightFrustum $ lightFrustum
            { frustumEye = xyz__ $ frustumView eyeFrustum `mul` (let Vec3 x y z = frustumEye lightFrustum in Vec4 x y z 1)
            , frustumViewProj = frustumViewProj lightFrustum `mul` frustumInvView eyeFrustum
            }
          renderUniform usLight uLightColor lightColor
          renderUploadUniformStorage usLight
          renderUniformStorage usLight

          renderDepthTestFunc DepthTestFuncAlways

          renderSampler 0 shadowRTT nullSamplerState

          renderProgram lightProgram
          renderScreenQuad screenQuadRenderer

          renderUniform usLight uLightColor ambientLightColor
          renderUploadUniformStorage usLight
          renderProgram ambientLightProgram
          renderScreenQuad screenQuadRenderer

      -- tone mapping
      renderScope $ do
        renderClearColor 0 $ Vec4 0 0 0 0
        renderSampler 0 colorRTT nullSamplerState
        renderDepthTestFunc DepthTestFuncAlways
        renderProgram toneMappingProgram
        renderScreenQuad screenQuadRenderer

      -- ui
      renderScope $ do
        renderDepthTestFunc DepthTestFuncAlways
        windowRender

    exit <- atomically $ readTVar exitVar
    when exit exitApp

  -- save config
  B.writeFile configFileName . S.encode =<< readTVarIO editorStateVar

errorHandler :: String -> SomeException -> IO ()
errorHandler s e = print (s, e)
