{-|
Module: Main
Description: Entry point for flaw model-editor executable.
License: MIT
-}

{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Main(main) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
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
	, editorStateTexture :: !(TextureId AppGraphicsDevice)
	}

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
main = handle errorHandler $ withBook $ \bk -> do
	-- init app
	(window, device, context, presenter, inputManager) <- book bk $ initApp $ appConfig
		{ appConfigTitle = "Flaw Model Editor"
		, appConfigNeedDepthBuffer = False
		}

	-- UI styles and drawer
	let metrics = defaultStyleMetrics
	drawer <- book bk $ initDefaultStyleDrawer device

	exitVar <- newTVarIO False

	-- editor state
	editorStateVar <- do
		initialGeometry <- book bk $ do
			let f alpha beta = VertexPNT
				{ f_VertexPNT_position = p
				, f_VertexPNT_normal = p
				, f_VertexPNT_texcoord = Vec2 (alpha / (pi * 2)) (beta / pi + 0.5)
				}
				where p = Vec3 (cos alpha * cos beta) (sin alpha * cos beta) (sin beta)
			loadPackedGeometry device =<< packGeometry (sphereVertices f 16 16)
		initialTexture <- book bk $ do
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
		newTVarIO EditorState
			{ editorStateEyePosition = Vec3 (-2) 0 1
			, editorStateEyeSpeed = Vec3 0 0 0
			, editorStateEyeAlpha = 0
			, editorStateEyeBeta = 0
			, editorStateMaterial = Vec4 0.2 0.5 0 0.4
			, editorStateLightPosition = Vec3 (-2) 2 2
			, editorStateLightColor = vecFromScalar 10
			, editorStateAmbientLightColor = vecFromScalar 0.01
			, editorStateGeometry = initialGeometry
			, editorStateTexture = initialTexture
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
	-- opaque program
	opaqueProgram <- book bk $ createProgram device $ do
		let v = undefined :: VertexPNT
		aPosition <- vertexAttribute 0 0 $ vertexPositionAttribute v
		aNormal <- vertexAttribute 0 0 $ vertexNormalAttribute v
		aTexcoord <- vertexAttribute 0 0 $ vertexTexcoordAttribute v
		worldPosition <- temp $ mul uWorld $ cvec31 aPosition (constf 1)
		viewNormal <- temp $ xyz__ $ mul uView $ cvec31 aNormal (constf 0)
		rasterize (mul uViewProj worldPosition) $ do
			albedo <- temp $ sample (sampler2D3f 0) aTexcoord
			let emission = 0
			let occlusion = 1
			outputDeferredPipelineOpaquePass (albedo * vecFromScalar emission) (cvec31 albedo occlusion) uMaterial viewNormal
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
				atomically $ modifyTVar' editorStateVar $ \state -> state
					{ editorStateGeometry = geometry
					}
			Left err -> do
				putStrLn $ T.unpack err

	let loadTextureFile fileName = handle errorHandler $ do
		bytes <- B.readFile $ T.unpack fileName
		PackedTexture
			{ packedTextureBytes = textureBytes
			, packedTextureInfo = textureInfo
			} <- loadTexture bytes
		(compressedTextureInfo, compressedTextureBytes) <- dxtCompressTexture textureInfo textureBytes
		texture <- book bk $ createStaticTexture device compressedTextureInfo defaultSamplerStateInfo compressedTextureBytes
		atomically $ modifyTVar' editorStateVar $ \state -> state
			{ editorStateTexture = texture
			}

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
	mainWindow@Window
		{ windowKeyboardState = keyboardState
		} <- atomically $ do
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
		textureFileElement <- newFileElement metrics
		setClickHandler textureFileElement $ do
			fileName <- getText textureFileElement
			asyncRunInFlow flow $ loadTextureFile fileName
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
		specularSlider <- newSlider metrics 0.01
		setFloatValue specularSlider $ y_ initialMaterial
		setChangeHandler specularSlider $ do
			value <- getFloatValue specularSlider
			modifyTVar' editorStateVar $ \s -> s
				{ editorStateMaterial = editorStateMaterial s * Vec4 1 0 1 1 + Vec4 0 value 0 0
				}
		metalnessSlider <- newSlider metrics 0.01
		setFloatValue metalnessSlider $ z_ initialMaterial
		setChangeHandler metalnessSlider $ do
			value <- getFloatValue metalnessSlider
			modifyTVar' editorStateVar $ \s -> s
				{ editorStateMaterial = editorStateMaterial s * Vec4 1 1 0 1 + Vec4 0 0 value 0
				}
		glossinessSlider <- newSlider metrics 0.01
		setFloatValue glossinessSlider $ w_ initialMaterial
		setChangeHandler glossinessSlider $ do
			value <- getFloatValue glossinessSlider
			modifyTVar' editorStateVar $ \s -> s
				{ editorStateMaterial = editorStateMaterial s * Vec4 1 1 1 0 + Vec4 0 0 0 value
				}
		propertiesFrame <- frameFlowLayout metrics $ do
			labeledFlowLayout "collada file" $ elementInFlowLayout colladaFileElement
			labeledFlowLayout "collada node" $ elementInFlowLayout colladaNodeEditBox
			labeledFlowLayout "texture file" $ elementInFlowLayout textureFileElement
			labeledFlowLayout "light" $ elementInFlowLayout lightIntensitySlider
			labeledFlowLayout "ambient light" $ elementInFlowLayout ambientLightIntensitySlider
			labeledFlowLayout "diffuse" $ elementInFlowLayout diffuseSlider
			labeledFlowLayout "specular" $ elementInFlowLayout specularSlider
			labeledFlowLayout "metalness" $ elementInFlowLayout metalnessSlider
			labeledFlowLayout "glossiness" $ elementInFlowLayout glossinessSlider
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

		return mainWindow

	-- run
	runApp $ \frameTime -> do
		-- process input and window events
		processWindow mainWindow

		-- process camera
		liftIO $ atomically $ do
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
				, editorStateTexture = tObject
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

				renderProgram opaqueProgram
				renderUniformStorage usObject
				renderVertexBuffer 0 vbObject
				renderIndexBuffer ibObject
				renderSampler 0 tObject nullSamplerState
				renderUniform usObject uWorld (affineIdentity :: Float4x4)
				renderUniform usObject uMaterial material
				renderUploadUniformStorage usObject
				renderUniformStorage usObject
				renderDraw icObject

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
