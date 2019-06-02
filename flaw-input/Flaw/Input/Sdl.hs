{-|
Module: Flaw.Input.Sdl
Description: User input for SDL.
License: MIT
-}

{-# LANGUAGE MultiParamTypeClasses, ViewPatterns #-}

module Flaw.Input.Sdl
  ( SdlInputManager(..)
  , SdlGamepad(..)
  , initSdlInput
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Unsafe as B
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Foreign.Marshal.Array
import Foreign.Ptr
import qualified SDL.Raw.Enum as SDL
import qualified SDL.Raw.Event as SDL
import qualified SDL.Raw.Types as SDL

import Flaw.Input
import Flaw.Input.Basic
import Flaw.Input.Gamepad
import Flaw.Input.Keyboard
import Flaw.Input.Mouse
import Flaw.Window.Sdl

keyFromSdlScancode :: SDL.Scancode -> Key
keyFromSdlScancode scancode = case scancode of
  SDL.SDL_SCANCODE_A -> KeyA
  SDL.SDL_SCANCODE_B -> KeyB
  SDL.SDL_SCANCODE_C -> KeyC
  SDL.SDL_SCANCODE_D -> KeyD
  SDL.SDL_SCANCODE_E -> KeyE
  SDL.SDL_SCANCODE_F -> KeyF
  SDL.SDL_SCANCODE_G -> KeyG
  SDL.SDL_SCANCODE_H -> KeyH
  SDL.SDL_SCANCODE_I -> KeyI
  SDL.SDL_SCANCODE_J -> KeyJ
  SDL.SDL_SCANCODE_K -> KeyK
  SDL.SDL_SCANCODE_L -> KeyL
  SDL.SDL_SCANCODE_M -> KeyM
  SDL.SDL_SCANCODE_N -> KeyN
  SDL.SDL_SCANCODE_O -> KeyO
  SDL.SDL_SCANCODE_P -> KeyP
  SDL.SDL_SCANCODE_Q -> KeyQ
  SDL.SDL_SCANCODE_R -> KeyR
  SDL.SDL_SCANCODE_S -> KeyS
  SDL.SDL_SCANCODE_T -> KeyT
  SDL.SDL_SCANCODE_U -> KeyU
  SDL.SDL_SCANCODE_V -> KeyV
  SDL.SDL_SCANCODE_W -> KeyW
  SDL.SDL_SCANCODE_X -> KeyX
  SDL.SDL_SCANCODE_Y -> KeyY
  SDL.SDL_SCANCODE_Z -> KeyZ
  SDL.SDL_SCANCODE_1 -> Key1
  SDL.SDL_SCANCODE_2 -> Key2
  SDL.SDL_SCANCODE_3 -> Key3
  SDL.SDL_SCANCODE_4 -> Key4
  SDL.SDL_SCANCODE_5 -> Key5
  SDL.SDL_SCANCODE_6 -> Key6
  SDL.SDL_SCANCODE_7 -> Key7
  SDL.SDL_SCANCODE_8 -> Key8
  SDL.SDL_SCANCODE_9 -> Key9
  SDL.SDL_SCANCODE_0 -> Key0
  SDL.SDL_SCANCODE_RETURN -> KeyReturn
  SDL.SDL_SCANCODE_ESCAPE -> KeyEscape
  SDL.SDL_SCANCODE_BACKSPACE -> KeyBackSpace
  SDL.SDL_SCANCODE_TAB -> KeyTab
  SDL.SDL_SCANCODE_SPACE -> KeySpace
  SDL.SDL_SCANCODE_CAPSLOCK -> KeyCapsLock
  SDL.SDL_SCANCODE_F1 -> KeyF1
  SDL.SDL_SCANCODE_F2 -> KeyF2
  SDL.SDL_SCANCODE_F3 -> KeyF3
  SDL.SDL_SCANCODE_F4 -> KeyF4
  SDL.SDL_SCANCODE_F5 -> KeyF5
  SDL.SDL_SCANCODE_F6 -> KeyF6
  SDL.SDL_SCANCODE_F7 -> KeyF7
  SDL.SDL_SCANCODE_F8 -> KeyF8
  SDL.SDL_SCANCODE_F9 -> KeyF9
  SDL.SDL_SCANCODE_F10 -> KeyF10
  SDL.SDL_SCANCODE_F11 -> KeyF11
  SDL.SDL_SCANCODE_F12 -> KeyF12
  SDL.SDL_SCANCODE_LSHIFT -> KeyShiftL
  SDL.SDL_SCANCODE_RSHIFT -> KeyShiftR
  SDL.SDL_SCANCODE_LCTRL -> KeyControlL
  SDL.SDL_SCANCODE_RCTRL -> KeyControlR
  SDL.SDL_SCANCODE_SCROLLLOCK -> KeyScrollLock
  SDL.SDL_SCANCODE_PAUSE -> KeyPause
  SDL.SDL_SCANCODE_INSERT -> KeyInsert
  SDL.SDL_SCANCODE_HOME -> KeyHome
  SDL.SDL_SCANCODE_PAGEUP -> KeyPageUp
  SDL.SDL_SCANCODE_DELETE -> KeyDelete
  SDL.SDL_SCANCODE_END -> KeyEnd
  SDL.SDL_SCANCODE_PAGEDOWN -> KeyPageDown
  SDL.SDL_SCANCODE_RIGHT -> KeyRight
  SDL.SDL_SCANCODE_LEFT -> KeyLeft
  SDL.SDL_SCANCODE_DOWN -> KeyDown
  SDL.SDL_SCANCODE_UP -> KeyUp
  SDL.SDL_SCANCODE_KP_DIVIDE -> KeyPadDivide
  SDL.SDL_SCANCODE_KP_MULTIPLY -> KeyPadMultiply
  SDL.SDL_SCANCODE_KP_MINUS -> KeyPadSubtract
  SDL.SDL_SCANCODE_KP_PLUS -> KeyPadAdd
  SDL.SDL_SCANCODE_KP_ENTER -> KeyPadEnter
  SDL.SDL_SCANCODE_KP_1 -> KeyPad1
  SDL.SDL_SCANCODE_KP_2 -> KeyPad2
  SDL.SDL_SCANCODE_KP_3 -> KeyPad3
  SDL.SDL_SCANCODE_KP_4 -> KeyPad4
  SDL.SDL_SCANCODE_KP_5 -> KeyPad5
  SDL.SDL_SCANCODE_KP_6 -> KeyPad6
  SDL.SDL_SCANCODE_KP_7 -> KeyPad7
  SDL.SDL_SCANCODE_KP_8 -> KeyPad8
  SDL.SDL_SCANCODE_KP_9 -> KeyPad9
  SDL.SDL_SCANCODE_KP_0 -> KeyPad0
  _ -> KeyUnknown

gamepadButtonFromSdlButton :: SDL.GameControllerButton -> GamepadButton
gamepadButtonFromSdlButton button = case button of
  SDL.SDL_CONTROLLER_BUTTON_A -> GamepadButtonA
  SDL.SDL_CONTROLLER_BUTTON_B -> GamepadButtonB
  SDL.SDL_CONTROLLER_BUTTON_X -> GamepadButtonX
  SDL.SDL_CONTROLLER_BUTTON_Y -> GamepadButtonY
  SDL.SDL_CONTROLLER_BUTTON_BACK -> GamepadButtonBack
  SDL.SDL_CONTROLLER_BUTTON_GUIDE -> GamepadButtonGuide
  SDL.SDL_CONTROLLER_BUTTON_START -> GamepadButtonStart
  SDL.SDL_CONTROLLER_BUTTON_LEFTSTICK -> GamepadButtonLeftStick
  SDL.SDL_CONTROLLER_BUTTON_RIGHTSTICK -> GamepadButtonRightStick
  SDL.SDL_CONTROLLER_BUTTON_LEFTSHOULDER -> GamepadButtonLeftShoulder
  SDL.SDL_CONTROLLER_BUTTON_RIGHTSHOULDER -> GamepadButtonRightShoulder
  SDL.SDL_CONTROLLER_BUTTON_DPAD_UP -> GamepadButtonDPadUp
  SDL.SDL_CONTROLLER_BUTTON_DPAD_DOWN -> GamepadButtonDPadDown
  SDL.SDL_CONTROLLER_BUTTON_DPAD_LEFT -> GamepadButtonDPadLeft
  SDL.SDL_CONTROLLER_BUTTON_DPAD_RIGHT -> GamepadButtonDPadRight
  _ -> GamepadButtonUnknown

gamepadAxisFromSdlAxis :: SDL.GameControllerAxis -> GamepadAxis
gamepadAxisFromSdlAxis axis = case axis of
  SDL.SDL_CONTROLLER_AXIS_LEFTX -> GamepadAxisLeftX
  SDL.SDL_CONTROLLER_AXIS_LEFTY -> GamepadAxisLeftY
  SDL.SDL_CONTROLLER_AXIS_RIGHTX -> GamepadAxisRightX
  SDL.SDL_CONTROLLER_AXIS_RIGHTY -> GamepadAxisRightY
  SDL.SDL_CONTROLLER_AXIS_TRIGGERLEFT -> GamepadAxisTriggerLeft
  SDL.SDL_CONTROLLER_AXIS_TRIGGERRIGHT -> GamepadAxisTriggerRight
  _ -> GamepadAxisUnknown

data SdlInputManager = SdlInputManager
  { sdlInputManagerBasic :: {-# UNPACK #-} !BasicInputManager
  , sdlInputManagerGamepadsVar :: {-# UNPACK #-} !(TVar (HM.HashMap SDL.JoystickID SdlGamepad))
  }

instance InputManager SdlInputManager KeyboardEvent where
  chanInputEvents = chanInputEvents . sdlInputManagerBasic

instance InputManager SdlInputManager MouseEvent where
  chanInputEvents = chanInputEvents . sdlInputManagerBasic

data SdlGamepad = SdlGamepad
  { sdlGamepadGameController :: {-# UNPACK #-} !SDL.GameController
  , sdlGamepadChan :: {-# UNPACK #-} !(TChan GamepadEvent)
  }

initSdlInput :: SdlWindow -> IO SdlInputManager
initSdlInput window = do
  -- init basic manager
  basicInputManager@BasicInputManager
    { mKeyboardChan = keyboardChan
    , mMouseChan = mouseChan
    } <- initBasicInputManager

  -- init gamepads var
  gamepadsVar <- newTVarIO HM.empty

  -- helper routines
  let
    addKeyboardEvent = atomically . writeTChan keyboardChan
    addMouseEvent = atomically . writeTChan mouseChan
    addGamepadEvent joystickId event = atomically $ do
      gamepads <- readTVar gamepadsVar
      case HM.lookup joystickId gamepads of
        Just SdlGamepad
          { sdlGamepadChan = chan
          } -> writeTChan chan event
        Nothing -> return ()

  addSdlWindowCallback window $ \event -> case event of

    SDL.KeyboardEvent
      { SDL.eventType = eventType
      , SDL.keyboardEventKeysym = SDL.Keysym
        { SDL.keysymScancode = scancode
        }
      } -> do
      let key = keyFromSdlScancode scancode
      case eventType of
        SDL.SDL_KEYDOWN -> addKeyboardEvent $ KeyDownEvent key
        SDL.SDL_KEYUP -> addKeyboardEvent $ KeyUpEvent key
        _ -> return ()

    SDL.TextInputEvent
      { SDL.textInputEventText = text
      } -> withArrayLen text $ \len ptr -> do
      bytes <- B.unsafePackCStringLen (ptr, len)
      case T.decodeUtf8' bytes of
        Left _e -> return ()
        Right t -> unless (T.length t == 0) $ do
          char <- evaluate $ T.head t
          addKeyboardEvent $ CharEvent char

    SDL.MouseButtonEvent
      { SDL.eventType = eventType
      , SDL.mouseButtonEventButton = sdlButton
      , SDL.mouseButtonEventX = x
      , SDL.mouseButtonEventY = y
      } -> do
      addMouseEvent $ CursorMoveEvent (fromIntegral x) (fromIntegral y)
      let
        maybeButton = case sdlButton of
          SDL.SDL_BUTTON_LEFT -> Just LeftMouseButton
          SDL.SDL_BUTTON_RIGHT -> Just RightMouseButton
          SDL.SDL_BUTTON_MIDDLE -> Just MiddleMouseButton
          _ -> Nothing
      case maybeButton of
        Just button -> case eventType of
          SDL.SDL_MOUSEBUTTONDOWN -> addMouseEvent $ MouseDownEvent button
          SDL.SDL_MOUSEBUTTONUP -> addMouseEvent $ MouseUpEvent button
          _ -> return ()
        Nothing -> return ()

    SDL.MouseMotionEvent
      { SDL.mouseMotionEventX = x
      , SDL.mouseMotionEventY = y
      , SDL.mouseMotionEventXRel = xrel
      , SDL.mouseMotionEventYRel = yrel
      } -> do
      addMouseEvent $ RawMouseMoveEvent (fromIntegral xrel) (fromIntegral yrel) 0
      addMouseEvent $ CursorMoveEvent (fromIntegral x) (fromIntegral y)

    SDL.MouseWheelEvent
      { SDL.mouseWheelEventY = y
      } -> addMouseEvent $ RawMouseMoveEvent 0 0 $ fromIntegral y

    SDL.ControllerDeviceEvent
      { SDL.eventType = eventType
      , SDL.controllerDeviceEventWhich = deviceId
      } -> case eventType of
      SDL.SDL_CONTROLLERDEVICEADDED -> do
        gameController <- SDL.gameControllerOpen (fromIntegral deviceId)
        when (gameController /= nullPtr) $ do
          joystick <- SDL.gameControllerGetJoystick gameController
          when (joystick /= nullPtr) $ do
            joystickId <- SDL.joystickInstanceID joystick
            chan <- newTChanIO
            atomically $ modifyTVar' gamepadsVar $ HM.insert joystickId SdlGamepad
              { sdlGamepadGameController = gameController
              , sdlGamepadChan = chan
              }
      SDL.SDL_CONTROLLERDEVICEREMOVED -> join $ atomically $ do
        gamepads <- readTVar gamepadsVar
        case HM.lookup deviceId gamepads of
          Just SdlGamepad
            { sdlGamepadGameController = gameController
            } -> do
            writeTVar gamepadsVar $! HM.delete deviceId gamepads
            return $ SDL.gameControllerClose gameController
          Nothing -> return $ return ()
      _ -> return ()

    SDL.ControllerButtonEvent
      { SDL.eventType = eventType
      , SDL.controllerButtonEventWhich = joystickId
      , SDL.controllerButtonEventButton = gamepadButtonFromSdlButton . fromIntegral -> button
      } -> when (button /= GamepadButtonUnknown) $ case eventType of
      SDL.SDL_CONTROLLERBUTTONDOWN -> addGamepadEvent joystickId $ GamepadButtonDownEvent button
      SDL.SDL_CONTROLLERBUTTONUP -> addGamepadEvent joystickId $ GamepadButtonUpEvent button
      _ -> return ()

    SDL.ControllerAxisEvent
      { SDL.eventType = SDL.SDL_CONTROLLERAXISMOTION
      , SDL.controllerAxisEventWhich = joystickId
      , SDL.controllerAxisEventAxis = gamepadAxisFromSdlAxis . fromIntegral -> axis
      , SDL.controllerAxisEventValue = value
      } -> when (axis /= GamepadAxisUnknown) $ addGamepadEvent joystickId $ GamepadAxisEvent axis (fromIntegral value * (1 / 0x8000))

    _ -> return ()

  return SdlInputManager
    { sdlInputManagerBasic = basicInputManager
    , sdlInputManagerGamepadsVar = gamepadsVar
    }
