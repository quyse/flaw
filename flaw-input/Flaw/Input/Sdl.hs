{-|
Module: Flaw.Input.Sdl
Description: User input for SDL.
License: MIT
-}

module Flaw.Input.Sdl
	( SdlInputManager()
	, initSdlInput
	) where

import Control.Concurrent.STM
import Control.Exception
import qualified Data.ByteString.Unsafe as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Foreign.Marshal.Array
import qualified SDL.Raw.Enum as SDL
import qualified SDL.Raw.Types as SDL

import Flaw.Input.Basic
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

type SdlInputManager = BasicInputManager

initSdlInput :: SdlWindow -> IO SdlInputManager
initSdlInput window = do
	-- init basic manager
	inputManager@BasicInputManager
		{ mKeyboardChan = keyboardChan
		, mMouseChan = mouseChan
		} <- initBasicInputManager

	addSdlWindowCallback window $ \event -> do
		-- helper routines
		let addKeyboardEvent e = atomically $ writeTChan keyboardChan e
		let addMouseEvent e = atomically $ writeTChan mouseChan e

		case event of
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
					Right t -> if T.length t == 0 then return () else do
						char <- evaluate $ T.head t
						addKeyboardEvent $ CharEvent char
			SDL.MouseButtonEvent
				{ SDL.eventType = eventType
				, SDL.mouseButtonEventButton = sdlButton
				} -> do
				let maybeButton = case sdlButton of
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
			_ -> return ()

	return inputManager
