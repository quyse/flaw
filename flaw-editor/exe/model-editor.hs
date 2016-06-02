{-|
Module: Main
Description: Entry point for flaw model-editor executable.
License: MIT
-}

{-# LANGUAGE OverloadedStrings, PatternSynonyms, RankNTypes #-}

module Main(main) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Vector as V

import Flaw.App
import Flaw.Asset.Collada
import Flaw.Asset.Texture.Dxt
import Flaw.Book
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
import Flaw.UI.DefaultStyle
import Flaw.UI.Drawer
import Flaw.UI.Editor.Elements
import Flaw.UI.Elements
import Flaw.UI.Layout
import Flaw.UI.RenderBox
import Flaw.Visual
import Flaw.Visual.Geometry
import Flaw.Visual.Geometry.Basic
import Flaw.Visual.Geometry.Vertex
import Flaw.Visual.Pipeline
import Flaw.Visual.Pipeline.Deferred
import Flaw.Visual.ScreenQuad
import Flaw.Visual.Texture
import Flaw.Window

data TextureCell = TextureCell
	{ textureCellBook :: !Book
	, textureCellTexture :: !(TextureId AppGraphicsDevice)
	}

newTextureCell :: IO (TextureId AppGraphicsDevice, IO ()) -> IO (TextureCell, IO ())
newTextureCell createTexture = withSpecialBook $ \bk -> do
	cellBook <- book bk newDynamicBook
	cellTexture <- book cellBook createTexture
	return TextureCell
		{ textureCellBook = cellBook
		, textureCellTexture = cellTexture
		}

data EditorState = EditorState
	{ editorStateEyePosition :: !Float3
	, editorStateEyeSpeed :: !Float3
	, editorStateEyeAlpha :: !Float
	, editorStateEyeBeta :: !Float
	, editorStateMaterial :: !Float4
	, editorStateLightPosition :: !Float3
	, editorStateLightColor :: !Float3
	, editorStateAmbientLightColor :: !Float3
	, editorStateGeometry :: !(Geometry AppGraphicsDevice)
	, editorStateAlbedoTextureCell :: !TextureCell
	, editorStateNormalTextureCell :: !TextureCell
	, editorStateNormalTextureEnabled :: !Bool
	, editorStateDiffuseTextureCell :: !TextureCell
	, editorStateDiffuseTextureEnabled :: !Bool
	, editorStateSpecularTextureCell :: !TextureCell
	, editorStateSpecularTextureEnabled :: !Bool
	, editorStateMetalnessTextureCell :: !TextureCell
	, editorStateMetalnessTextureEnabled :: !Bool
	, editorStateGlossinessTextureCell :: !TextureCell
	, editorStateGlossinessTextureEnabled :: !Bool
	, editorStateLinearFiltering :: !Bool
	}

pattern SHADER_FLAG_NORMAL_MAP = 1
pattern SHADER_FLAG_DIFFUSE_MAP = 2
pattern SHADER_FLAG_SPECULAR_MAP = 4
pattern SHADER_FLAG_METALNESS_MAP = 8
pattern SHADER_FLAG_GLOSSINESS_MAP = 16
pattern SHADER_FLAGS_COUNT = 32

getEyeDirection :: EditorState -> Float3
getEyeDirection EditorState
	{ editorStateEyeAlpha = eyeAlpha
	, editorStateEyeBeta = eyeBeta
	} = Float3 (ca * cb) (sa * cb) sb where
	ca = cos eyeAlpha
	sa = sin eyeAlpha
	cb = cos eyeBeta
	sb = sin eyeBeta

main :: IO ()
main = withApp appConfig
	{ appConfigTitle = "Flaw Model Editor"
	, appConfigNeedDepthBuffer = False
	} $ \window device context presenter inputManager -> withBook $ \bk -> do

	-- UI styles and drawer
	let metrics = defaultStyleMetrics
	drawer <- book bk $ initDefaultStyleDrawer device

	exitVar <- newTVarIO False

	-- create sphere geometry
	sphereGeometry <- book bk $ do
		let f alpha beta = VertexPNT
			{ f_VertexPNT_position = p
			, f_VertexPNT_normal = p
			, f_VertexPNT_texcoord = Vec2 (alpha / (pi * 2)) (beta / pi + 0.5)
			}
			where p = Vec3 (cos alpha * cos beta) (sin alpha * cos beta) (sin beta)
		loadPackedGeometry device =<< packGeometry (sphereVertices f 16 16)

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
				} defaultSamplerStateInfo $ B.pack $ do
				i <- [0..(height - 1)]
				j <- [0..(width - 1)]
				let c = if (i `quot` step + j `quot` step) `rem` 2 == 0 then 255 else 0
				[c, c, c, 255]
		initialNormalTextureCell <- book bk $ newTextureCell $ do
			let
				width = 1024
				height = 1024
				step = 64
			(compressedTextureInfo, compressedTextureBytes) <- dxtCompressTexture TextureInfo
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
			createStaticTexture device compressedTextureInfo defaultSamplerStateInfo compressedTextureBytes
		let newGrayTextureCell = book bk $ newTextureCell $ do
			let
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
			createStaticTexture device textureInfo defaultSamplerStateInfo textureBytes
		initialDiffuseTextureCell <- newGrayTextureCell
		initialSpecularTextureCell <- newGrayTextureCell
		initialMetalnessTextureCell <- newGrayTextureCell
		initialGlossinessTextureCell <- newGrayTextureCell
		newTVarIO EditorState
			{ editorStateEyePosition = Vec3 (-2) 0 1
			, editorStateEyeSpeed = Vec3 0 0 0
			, editorStateEyeAlpha = 0
			, editorStateEyeBeta = 0
			, editorStateMaterial = Vec4 0.2 0.5 0 0.4
			, editorStateLightPosition = Vec3 (-2) 2 2
			, editorStateLightColor = vecFromScalar 10
			, editorStateAmbientLightColor = vecFromScalar 0.01
			, editorStateGeometry = sphereGeometry
			, editorStateAlbedoTextureCell = initialAlbedoTextureCell
			, editorStateNormalTextureCell = initialNormalTextureCell
			, editorStateNormalTextureEnabled = False
			, editorStateDiffuseTextureCell = initialDiffuseTextureCell
			, editorStateDiffuseTextureEnabled = False
			, editorStateSpecularTextureCell = initialSpecularTextureCell
			, editorStateSpecularTextureEnabled = False
			, editorStateMetalnessTextureCell = initialMetalnessTextureCell
			, editorStateMetalnessTextureEnabled = False
			, editorStateGlossinessTextureCell = initialGlossinessTextureCell
			, editorStateGlossinessTextureEnabled = False
			, editorStateLinearFiltering = False
			}

	-- render pipeline
	updatePipeline <- book bk $ newPipelineWrapper $ newDeferredPipeline device

	-- programs
	ubsEye <- uniformBufferSlot 0
	uView <- uniform ubsEye
	uViewProj <- uniform ubsEye
	uInvProj <- uniform ubsEye
	ubsLight <- uniformBufferSlot 1
	uLightPosition <- uniform ubsLight
	uLightColor <- uniform ubsLight
	ubsObject <- uniformBufferSlot 2
	uWorld <- uniform ubsObject
	uMaterial <- uniform ubsObject

	usEye <- book bk $ createUniformStorage device ubsEye
	usLight <- book bk $ createUniformStorage device ubsLight
	usObject <- book bk $ createUniformStorage device ubsObject
	-- opaque programs
	opaquePrograms <- V.generateM SHADER_FLAGS_COUNT $ \shaderFlags -> book bk $ createProgram device $ do
		let v = undefined :: VertexPNT
		aPosition <- vertexAttribute 0 0 $ vertexPositionAttribute v
		aNormal <- vertexAttribute 0 0 $ vertexNormalAttribute v
		aTexcoord <- vertexAttribute 0 0 $ vertexTexcoordAttribute v
		worldPosition <- temp $ mul uWorld $ cvec31 aPosition (constf 1)
		viewPosition <- temp $ uView `mul` worldPosition
		vertexViewNormal <- temp $ xyz__ $ mul uView $ cvec31 aNormal (constf 0)
		rasterize (uViewProj `mul` worldPosition) $ do
			albedo <- temp $ sample (sampler2D3f 0) aTexcoord
			resultViewNormal <- if (shaderFlags .&. SHADER_FLAG_NORMAL_MAP) > 0 then do
				(viewTangent, viewBinormal, viewNormal) <- tangentFrame (xyz__ viewPosition) vertexViewNormal aTexcoord
				bentNormalXY <- temp $ sample (sampler2D2f 1) aTexcoord * vecFromScalar (255 / 127) - vecFromScalar 1
				bentNormal <- temp $ cvec21 bentNormalXY $ sqrt $ max_ 0 $ 1 - dot bentNormalXY bentNormalXY
				temp $ normalize $ viewTangent * xxx__ bentNormal + viewBinormal * yyy__ bentNormal + viewNormal * zzz__ bentNormal
				else temp $ normalize vertexViewNormal
			let
				emission = 0
				occlusion = 1
				diffuse    = if (shaderFlags .&. SHADER_FLAG_DIFFUSE_MAP    ) > 0 then sample (sampler2Df 2) aTexcoord else x_ uMaterial
				specular   = if (shaderFlags .&. SHADER_FLAG_SPECULAR_MAP   ) > 0 then sample (sampler2Df 3) aTexcoord else y_ uMaterial
				metalness  = if (shaderFlags .&. SHADER_FLAG_METALNESS_MAP  ) > 0 then sample (sampler2Df 4) aTexcoord else z_ uMaterial
				glossiness = if (shaderFlags .&. SHADER_FLAG_GLOSSINESS_MAP ) > 0 then sample (sampler2Df 5) aTexcoord else w_ uMaterial
			outputDeferredPipelineOpaquePass (albedo * vecFromScalar emission) (cvec31 albedo occlusion) (cvec1111 diffuse specular metalness glossiness) resultViewNormal
	-- lightbulb opaque program
	lightbulbOpaqueProgram <- book bk $ createProgram device $ do
		let v = undefined :: VertexPNT
		aPosition <- vertexAttribute 0 0 $ vertexPositionAttribute v
		aNormal <- vertexAttribute 0 0 $ vertexNormalAttribute v
		worldPosition <- temp $ mul uWorld $ cvec31 aPosition (constf 1)
		vertexViewNormal <- temp $ xyz__ $ mul uView $ cvec31 aNormal (constf 0)
		rasterize (uViewProj `mul` worldPosition) $ do
			viewNormal <- temp $ normalize vertexViewNormal
			outputDeferredPipelineOpaquePass uLightColor (const4f $ Vec4 0 0 0 0) (const4f $ Vec4 0 0 0 0) viewNormal
	-- light program
	screenQuadRenderer <- book bk $ newScreenQuadRenderer device
	lightProgram <- book bk $ createProgram device $ deferredPipelineLightPassProgram uInvProj $ \viewPosition -> do
		toLight <- temp $ uLightPosition - viewPosition
		toLightDirection <- temp $ normalize toLight
		lightColor <- temp $ uLightColor / vecFromScalar (dot toLight toLight)
		return (toLightDirection, lightColor)
	-- ambient light program
	ambientLightProgram <- book bk $ createProgram device $ deferredPipelineAmbientLightPassProgram uInvProj uLightColor
	-- tone mapping
	toneMappingProgram <- book bk $ createProgram device $ screenQuadProgram $ \screenPositionTexcoord -> do
		xyY <- xyz2xyY =<< rgb2xyz (sample (sampler2D3f 0) $ zw__ screenPositionTexcoord)
		rgb <- xyz2rgb =<< xyY2xyz (cvec111 (x_ xyY) (y_ xyY) (z_ xyY / (1 + z_ xyY)))
		colorTarget 0 $ cvec31 rgb 1

	linearSamplerState <- book bk $ createSamplerState device defaultSamplerStateInfo
		{ samplerMinFilter = SamplerLinearFilter
		, samplerMipFilter = SamplerLinearFilter
		, samplerMagFilter = SamplerLinearFilter
		}

	let cameraNearPlane = 0.01
	let cameraFarPlane = 1000

	-- flow for background operations
	flow <- book bk newFlow

	let loadColladaFile fileName elementId = handle errorHandler $ do
		bytes <- BL.readFile $ T.unpack fileName
		let eitherVertices = runCollada $ do
			initColladaCache bytes
			createColladaVertices =<< parseGeometry =<< getElementById elementId
		case eitherVertices of
			Right vertices -> do
				geometry <- book bk (loadPackedGeometry device =<< packGeometry (vertices :: V.Vector VertexPNT))
				atomically $ modifyTVar' editorStateVar $ \s -> s
					{ editorStateGeometry = geometry
					}
			Left err -> do
				putStrLn $ T.unpack err

	let loadTextureFile fileName = do
		bytes <- B.readFile $ T.unpack fileName
		PackedTexture
			{ packedTextureBytes = textureBytes
			, packedTextureInfo = textureInfo
			} <- loadTexture bytes
		(compressedTextureInfo, compressedTextureBytes) <- dxtCompressTexture textureInfo textureBytes
		createStaticTexture device compressedTextureInfo defaultSamplerStateInfo compressedTextureBytes

	-- input callback
	let inputCallback inputEvent InputState
		{ inputStateMouse = mouseInputState
		} = case inputEvent of
		KeyboardInputEvent keyboardEvent -> case keyboardEvent of
			KeyDownEvent KeyL -> do
				modifyTVar' editorStateVar $ \editorState@EditorState
					{ editorStateEyePosition = eyePosition
					} -> editorState
					{ editorStateLightPosition = eyePosition
					}
				return True
			_ -> return False
		MouseInputEvent mouseEvent -> case mouseEvent of
			MouseDownEvent LeftMouseButton -> do
				asyncRunInFlow flow $ setWindowMouseLock window True
				return True
			MouseUpEvent LeftMouseButton -> do
				asyncRunInFlow flow $ setWindowMouseLock window False
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

		-- properties frame
		colladaFileElement <- newFileElement metrics
		colladaNodeEditBox <- newEditBox
		setClickHandler colladaFileElement $ do
			fileName <- getText colladaFileElement
			nodeName <- getText colladaNodeEditBox
			asyncRunInFlow flow $ loadColladaFile fileName nodeName
		let textureFileElement getTextureCell setTextureCell = do
			fileElement <- newFileElement metrics
			setClickHandler fileElement $ do
				fileName <- getText fileElement
				asyncRunInFlow flow $ handle errorHandler $ do
					cell@TextureCell
						{ textureCellBook = cellBook
						} <- getTextureCell <$> readTVarIO editorStateVar
					freePreviousTexture <- releaseBook cellBook
					texture <- book cellBook $ loadTextureFile fileName
					atomically $ modifyTVar' editorStateVar $ setTextureCell cell
						{ textureCellTexture = texture
						}
					freePreviousTexture
			return fileElement
		albedoTextureFileElement <- textureFileElement editorStateAlbedoTextureCell $ \c s -> s
			{ editorStateAlbedoTextureCell = c
			}
		normalTextureFileElement <- textureFileElement editorStateNormalTextureCell $ \c s -> s
			{ editorStateNormalTextureCell = c
			}
		lightIntensitySlider <- newSlider metrics 0.01
		setFloatValue lightIntensitySlider 0.5
		setChangeHandler lightIntensitySlider $ do
			value <- getFloatValue lightIntensitySlider
			modifyTVar' editorStateVar $ \s -> s
				{ editorStateLightColor = vecFromScalar $ value * 10
				}
		ambientLightIntensitySlider <- newSlider metrics 0.01
		setFloatValue ambientLightIntensitySlider 0.1
		setChangeHandler ambientLightIntensitySlider $ do
			value <- getFloatValue ambientLightIntensitySlider
			modifyTVar' editorStateVar $ \s -> s
				{ editorStateAmbientLightColor = vecFromScalar $ value * 0.1
				}
		initialMaterial <- editorStateMaterial <$> readTVar editorStateVar
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
		linearFilteringCheckBox <- newLabeledCheckBox "enable"
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
				lift $ setChangeHandler checkBox $ do
					checked <- getChecked checkBox
					modifyTVar' editorStateVar $ \s -> s
						{ editorStateDiffuseTextureEnabled = checked
						}
			labeledFlowLayout "specular" $ elementInFlowLayout specularSlider
			checkBoxedFlowLayout "specular texture" $ \checkBox -> do
				elementInFlowLayout specularTextureFileElement
				lift $ setChangeHandler checkBox $ do
					checked <- getChecked checkBox
					modifyTVar' editorStateVar $ \s -> s
						{ editorStateSpecularTextureEnabled = checked
						}
			labeledFlowLayout "metalness" $ elementInFlowLayout metalnessSlider
			checkBoxedFlowLayout "metalness texture" $ \checkBox -> do
				elementInFlowLayout metalnessTextureFileElement
				lift $ setChangeHandler checkBox $ do
					checked <- getChecked checkBox
					modifyTVar' editorStateVar $ \s -> s
						{ editorStateMetalnessTextureEnabled = checked
						}
			labeledFlowLayout "glossiness" $ elementInFlowLayout glossinessSlider
			checkBoxedFlowLayout "glossiness texture" $ \checkBox -> do
				elementInFlowLayout glossinessTextureFileElement
				lift $ setChangeHandler checkBox $ do
					checked <- getChecked checkBox
					modifyTVar' editorStateVar $ \s -> s
						{ editorStateGlossinessTextureEnabled = checked
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
					{ editorStateEyePosition = position
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
				let acceleration = 5
				let direction = getEyeDirection editorState
				let forward = direction
				let rightward = normalize $ cross direction (Float3 0 0 1)
				let upward = normalize $ cross rightward forward
				let newSpeed = (speed + acceleration *
					( forward * vecFromScalar (frameTime * ((if w || up then 1 else 0) + (if s || down then (-1) else 0)))
					+ rightward * vecFromScalar (frameTime * ((if d || right then 1 else 0) + (if a || left then (-1) else 0)))
					+ upward * vecFromScalar (frameTime * ((if e then 1 else 0) + (if q then (-1) else 0)))
					)) * vecFromScalar (exp (log 0.01 * frameTime))
				let newSpeedNorm = norm newSpeed
				let maxSpeed = 10
				let newSpeed2 = if newSpeedNorm > maxSpeed then newSpeed * vecFromScalar (maxSpeed / newSpeedNorm) else newSpeed
				writeTVar editorStateVar editorState
					{ editorStateEyePosition = position + speed * vecFromScalar frameTime
					, editorStateEyeSpeed = newSpeed2
					}

		-- update drawer
		atomically $ setDrawerFrameTime drawer frameTime

		-- render window
		windowRender <- atomically $ renderWindow mainWindow drawer

		render context $ present presenter $ do

			editorState@EditorState
				{ editorStateEyePosition = eyePosition
				, editorStateMaterial = material
				, editorStateLightPosition = lightPosition
				, editorStateLightColor = lightColor
				, editorStateAmbientLightColor = ambientLightColor
				, editorStateGeometry = Geometry
					{ geometryVertexBuffer = vbObject
					, geometryIndexBuffer = ibObject
					, geometryIndicesCount = icObject
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
				, editorStateLinearFiltering = linearFiltering
				} <- liftIO $ readTVarIO editorStateVar

			deferredPipeline@DeferredPipeline
				{ deferredPipelineColorRTT = colorRTT
				} <- updatePipeline

			-- setup camera
			let eyeDirection = getEyeDirection editorState
			Vec4 viewportLeft viewportTop viewportRight viewportBottom <- renderGetViewport
			let viewportWidth = viewportRight - viewportLeft
			let viewportHeight = viewportBottom - viewportTop
			let aspect = fromIntegral viewportWidth / fromIntegral viewportHeight

			let view = affineLookAt eyePosition (eyePosition + eyeDirection) (Vec3 0 0 1) :: Float4x4
			let proj = projectionPerspectiveFov (pi / 4) aspect cameraFarPlane cameraNearPlane :: Float4x4
			let viewProj = mul proj view
			let invProj = matInverse proj

			renderUniform usEye uView view
			renderUniform usEye uViewProj viewProj
			renderUniform usEye uInvProj invProj
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
				renderUniform usObject uWorld (affineIdentity :: Float4x4)
				renderUniform usObject uMaterial material
				renderUploadUniformStorage usObject
				renderUniformStorage usObject
				renderDraw icObject

				-- render lightbulb
				do
					let Geometry
						{ geometryVertexBuffer = vb
						, geometryIndexBuffer = ib
						, geometryIndicesCount = ic
						} = sphereGeometry
					renderProgram lightbulbOpaqueProgram
					renderVertexBuffer 0 vb
					renderIndexBuffer ib
					renderUniform usObject uWorld (affineTranslation lightPosition `mul` affineScaling (vecFromScalar 0.05 :: Float3))
					renderUploadUniformStorage usObject
					renderUniformStorage usObject
					renderUniform usLight uLightColor lightColor
					renderUploadUniformStorage usLight
					renderUniformStorage usLight
					renderDraw ic

			-- lighting pass
			renderScope $ do
				renderDeferredPipelineLightPass deferredPipeline

				let
					Vec3 x y z = lightPosition
					lightPosition1 = Vec4 x y z 1
					in renderUniform usLight uLightPosition $ xyz__ $ view `mul` lightPosition1
				renderUniform usLight uLightColor lightColor
				renderUploadUniformStorage usLight
				renderUniformStorage usLight

				renderDepthTestFunc DepthTestFuncAlways

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

errorHandler :: SomeException -> IO ()
errorHandler = print
