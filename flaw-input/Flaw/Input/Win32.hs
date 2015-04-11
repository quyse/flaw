{-|
Module: Flaw.Input.Win32
Description: User input for Win32.
License: MIT
-}

{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Flaw.Input.Win32
	( Win32InputManager()
	, Win32InputFrame()
	, initWin32InputManager
	) where

import Control.Exception
import Control.Monad
import Data.Array.IO
import Data.Bits
import Data.IORef
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array(withArray)
import Foreign.Marshal.Utils(with)
import Foreign.Ptr
import Foreign.Storable

import Flaw.Exception
import Flaw.Input
import Flaw.FFI
import Flaw.FFI.Win32
import Flaw.Window.Win32

------- WinAPI declarations.

genStruct "RAWINPUTDEVICE"
	[ ([t| USHORT |], "usUsagePage")
	, ([t| USHORT |], "usUsage")
	, ([t| DWORD |], "dwFlags")
	, ([t| HWND |], "hwndTarget")
	]

genEnum [t| DWORD |] "RAWINPUT_DeviceType"
	[ ("RIM_TYPEMOUSE", 0)
	, ("RIM_TYPEKEYBOARD", 1)
	, ("RIM_TYPEHID", 2)
	]

genStruct "RAWMOUSE"
	[ ([t| USHORT |], "usFlags")
	, ([t| USHORT |], "gap")
	, ([t| USHORT |], "usButtonFlags")
	, ([t| SHORT |], "usButtonData")
	, ([t| ULONG |], "ulRawButtons")
	, ([t| LONG |], "lLastX")
	, ([t| LONG |], "lLastY")
	, ([t| ULONG |], "ulExtraInformation")
	]

genStruct "RAWKEYBOARD"
	[ ([t| USHORT |], "MakeCode")
	, ([t| USHORT |], "Flags")
	, ([t| USHORT |], "Reserved")
	, ([t| USHORT |], "VKey")
	, ([t| UINT |], "Message")
	, ([t| ULONG |], "ExtraInformation")
	]

genStruct "RAWHID"
	[ ([t| DWORD |], "dwSizeHid")
	, ([t| DWORD |], "dwCount")
	, ([t| BYTE |], "bRawData")
	]

genStruct "RAWINPUTHEADER"
	[ ([t| RAWINPUT_DeviceType |], "dwType")
	, ([t| DWORD |], "dwSize")
	, ([t| HANDLE |], "hDevice")
	, ([t| WPARAM |], "wParam")
	]

genStructWithEndUnion "RAWINPUT"
	-- [ ([t| RAWINPUTHEADER |], "header")
	-- Header is inlined here, to be able to use dwType as selector.
	[ ([t| RAWINPUT_DeviceType |], "dwType", 0)
	, ([t| DWORD |], "dwSize", 0)
	, ([t| HANDLE |], "hDevice", 0)
	, ([t| WPARAM |], "wParam", 0)
	] 0
	[ ("RIM_TYPEMOUSE", [t| RAWMOUSE |], "mouse")
	, ("RIM_TYPEKEYBOARD", [t| RAWKEYBOARD |], "keyboard")
	, ("RIM_TYPEHID", [t| RAWHID |], "hid")
	]

type HRAWINPUT = Ptr ()

foreign import stdcall safe "RegisterRawInputDevices" winapi_RegisterRawInputDevices
	:: Ptr RAWINPUTDEVICE -- pRawInputDevices
	-> UINT -- uiNumDevices
	-> UINT -- cbSize
	-> IO Bool

foreign import stdcall safe "GetRawInputData" winapi_GetRawInputData
	:: HRAWINPUT -- hRawInput
	-> UINT -- uiCommand
	-> Ptr () -- pData
	-> Ptr UINT -- pcbSize
	-> UINT -- cbSizeHeader
	-> IO UINT

------- end of WinAPI declarations

keyFromVKKey :: USHORT -> Key
keyFromVKKey k = case k of
	0x08 {- VK_BACK -} -> KeyBackSpace
	0x09 {- VK_TAB -} -> KeyTab
	0x0C {- VK_CLEAR -} -> KeyClear
	0x0D {- VK_RETURN -} -> KeyReturn
	0x10 {- VK_SHIFT -} -> KeyShiftL
	0x11 {- VK_CONTROL -} -> KeyControlL
	0x13 {- VK_PAUSE -} -> KeyPause
	0x91 {- VK_SCROLL -} -> KeyScrollLock
	0x1B {- VK_ESCAPE -} -> KeyEscape
	0x2D {- VK_INSERT -} -> KeyInsert
	0x2E {- VK_DELETE -} -> KeyDelete
	0x24 {- VK_HOME -} -> KeyHome
	0x25 {- VK_LEFT -} -> KeyLeft
	0x26 {- VK_UP -} -> KeyUp
	0x27 {- VK_RIGHT -} -> KeyRight
	0x28 {- VK_DOWN -} -> KeyDown
	0x21 {- VK_PRIOR -} -> KeyPageUp
	0x22 {- VK_NEXT -} -> KeyPageDown
	0x23 {- VK_END -} -> KeyEnd
	0x90 {- VK_NUMLOCK -} -> KeyNumLock
	0x60 {- VK_NUMPAD0 -} -> KeyPad0
	0x61 {- VK_NUMPAD1 -} -> KeyPad1
	0x62 {- VK_NUMPAD2 -} -> KeyPad2
	0x63 {- VK_NUMPAD3 -} -> KeyPad3
	0x64 {- VK_NUMPAD4 -} -> KeyPad4
	0x65 {- VK_NUMPAD5 -} -> KeyPad5
	0x66 {- VK_NUMPAD6 -} -> KeyPad6
	0x67 {- VK_NUMPAD7 -} -> KeyPad7
	0x68 {- VK_NUMPAD8 -} -> KeyPad8
	0x69 {- VK_NUMPAD9 -} -> KeyPad9
	0x70 {- VK_F1 -} -> KeyF1
	0x71 {- VK_F2 -} -> KeyF2
	0x72 {- VK_F3 -} -> KeyF3
	0x73 {- VK_F4 -} -> KeyF4
	0x74 {- VK_F5 -} -> KeyF5
	0x75 {- VK_F6 -} -> KeyF6
	0x76 {- VK_F7 -} -> KeyF7
	0x77 {- VK_F8 -} -> KeyF8
	0x78 {- VK_F9 -} -> KeyF9
	0x79 {- VK_F10 -} -> KeyF10
	0x7A {- VK_F11 -} -> KeyF11
	0x7B {- VK_F12 -} -> KeyF12
	0xA0 {- VK_LSHIFT -} -> KeyShiftL
	0xA1 {- VK_RSHIFT -} -> KeyShiftR
	0xA2 {- VK_LCONTROL -} -> KeyControlL
	0xA3 {- VK_RCONTROL -} -> KeyControlR
	0x14 {- VK_CAPITAL -} -> KeyCapsLock
	0xA4 {- VK_LMENU -} -> KeyAltL
	0xA5 {- VK_RMENU -} -> KeyAltR
	0x5B {- VK_LWIN -} -> KeySuperL
	0x5C {- VK_RWIN -} -> KeySuperR
	0x20 {- VK_SPACE -} -> KeySpace
	0x30 {- VK_0 -} -> Key0
	0x31 {- VK_1 -} -> Key1
	0x32 {- VK_2 -} -> Key2
	0x33 {- VK_3 -} -> Key3
	0x34 {- VK_4 -} -> Key4
	0x35 {- VK_5 -} -> Key5
	0x36 {- VK_6 -} -> Key6
	0x37 {- VK_7 -} -> Key7
	0x38 {- VK_8 -} -> Key8
	0x39 {- VK_9 -} -> Key9
	0x41 {- VK_A -} -> KeyA
	0x42 {- VK_B -} -> KeyB
	0x43 {- VK_C -} -> KeyC
	0x44 {- VK_D -} -> KeyD
	0x45 {- VK_E -} -> KeyE
	0x46 {- VK_F -} -> KeyF
	0x47 {- VK_G -} -> KeyG
	0x48 {- VK_H -} -> KeyH
	0x49 {- VK_I -} -> KeyI
	0x4A {- VK_J -} -> KeyJ
	0x4B {- VK_K -} -> KeyK
	0x4C {- VK_L -} -> KeyL
	0x4D {- VK_M -} -> KeyM
	0x4E {- VK_N -} -> KeyN
	0x4F {- VK_O -} -> KeyO
	0x50 {- VK_P -} -> KeyP
	0x51 {- VK_Q -} -> KeyQ
	0x52 {- VK_R -} -> KeyR
	0x53 {- VK_S -} -> KeyS
	0x54 {- VK_T -} -> KeyT
	0x55 {- VK_U -} -> KeyU
	0x56 {- VK_V -} -> KeyV
	0x57 {- VK_W -} -> KeyW
	0x58 {- VK_X -} -> KeyX
	0x59 {- VK_Y -} -> KeyY
	0x5A {- VK_Z -} -> KeyZ
	_ -> KeyUnknown

data Win32InputFrame = Win32InputFrame
	{ fKeys :: IOUArray Key Bool
	, fMouseButtons :: IOUArray MouseButton Bool
	, fCursor :: IORef (Int, Int)
	, fEvents :: IORef [Event]
	}

initWin32InputFrame :: IO Win32InputFrame
initWin32InputFrame = do
	keys <- newArray (minBound, maxBound) False
	mouseButtons <- newArray (minBound, maxBound) False
	cursor <- newIORef (0, 0)
	events <- newIORef []
	return Win32InputFrame
		{ fKeys = keys
		, fMouseButtons = mouseButtons
		, fCursor = cursor
		, fEvents = events
		}

instance State Win32InputFrame where
	getKeyState Win32InputFrame
		{ fKeys = keysArray
		} key = readArray keysArray key
	getMouseButtonState Win32InputFrame
		{ fMouseButtons = mouseButtonsArray
		} mouseButton = readArray mouseButtonsArray mouseButton
	getMouseCursor Win32InputFrame
		{ fCursor = cursorRef
		} = readIORef cursorRef

instance Frame Win32InputFrame where
	nextInputEvent frame@Win32InputFrame
		{ fEvents = eventsRef
		} = do
		events <- readIORef eventsRef
		case events of
			e:es -> do
				applyEventToState frame e
				writeIORef eventsRef es
				return $ Just e
			[] -> return Nothing

applyEventToState :: Win32InputFrame -> Event -> IO ()
applyEventToState Win32InputFrame
	{ fKeys = keysArray
	} (EventKeyboard keyboardEvent) = do
	case keyboardEvent of
		KeyDownEvent key -> writeArray keysArray key True
		KeyUpEvent key -> writeArray keysArray key False
		CharEvent _char -> return ()
applyEventToState Win32InputFrame
	{ fMouseButtons = mouseButtonsArray
	, fCursor = cursorRef
	} (EventMouse mouseEvent) = do
	case mouseEvent of
		MouseDownEvent mouseButton -> writeArray mouseButtonsArray mouseButton True
		MouseUpEvent mouseButton -> writeArray mouseButtonsArray mouseButton False
		RawMouseMoveEvent _x _y _z -> return ()
		CursorMoveEvent dx dy -> modifyIORef' cursorRef $ \(x, y) -> (x + dx, y + dy)

data Win32InputManager = Win32InputManager
	{ -- | Input frames (current and internal).
	  mFrames :: IORef (Win32InputFrame, Win32InputFrame)
	, mWindow :: Win32Window
	}

instance Manager Win32InputManager where
	type ManagerFrame Win32InputManager = Win32InputFrame
	nextInputFrame Win32InputManager
		{ mFrames = framesRef
		, mWindow = Win32Window
			{ wWindowSystem = windowSystem
			}
		} = do
		-- swap current and internal frames with syncronization
		invokeWin32WindowSystem windowSystem $ do
			-- get frames
			(currentFrame@Win32InputFrame
				{ fEvents = eventsRef
				}, internalFrame) <- readIORef framesRef
			-- rewind current frame (if there's some events left)
			events <- readIORef eventsRef
			forM_ events $ applyEventToState currentFrame
			writeIORef eventsRef []
			-- swap frames
			writeIORef framesRef (internalFrame, currentFrame)
		-- return current current frame
		(currentFrame, _internalFrame) <- readIORef framesRef
		return currentFrame

initWin32InputManager :: Win32Window -> IO Win32InputManager
initWin32InputManager window@Win32Window
	{ wHandle = hwnd
	} = do
	-- initialize frames
	framesRef <- do
		currentFrame <- initWin32InputFrame
		internalFrame <- initWin32InputFrame
		newIORef (currentFrame, internalFrame)

	-- add callback for windows messages
	addWin32WindowCallback window $ \msg wParam lParam -> do
		let addEvent event = do
			(_currentFrame, Win32InputFrame
				{ fEvents = eventsRef
				}) <- readIORef framesRef
			modifyIORef eventsRef (++ [event])
		let addEventWithFrame callback = do
			(_currentFrame, internalFrame@Win32InputFrame
				{ fEvents = eventsRef
				}) <- readIORef framesRef
			event <- callback internalFrame
			modifyIORef eventsRef (++ [event])

		case msg of
			0x0102 {- WM_CHAR -} -> do
				addEvent $ EventKeyboard $ CharEvent $ toEnum $ fromIntegral wParam
			0x0200 {- WM_MOUSEMOVE -} -> do
				addEventWithFrame $ \Win32InputFrame
					{ fCursor = cursorRef
					} -> do
					(cursorX, cursorY) <- readIORef cursorRef
					return $ EventMouse $ CursorMoveEvent
						(loWord (fromIntegral lParam) - cursorX)
						(hiWord (fromIntegral lParam) - cursorY)
			0x00FF {- WM_INPUT -} -> do
				let blockSize = sizeOf (undefined :: RAWINPUTHEADER) + 32
				allocaBytes blockSize $ \blockPtr -> do
					r <- with (fromIntegral blockSize) $ \blockSizePtr -> do
						winapi_GetRawInputData
							(intPtrToPtr $ fromIntegral lParam)
							(0x10000003 {- RID_INPUT -})
							blockPtr blockSizePtr (fromIntegral $ sizeOf (undefined :: RAWINPUTHEADER))
					if r > 0 then do
						let eventHeaderPtr = castPtr blockPtr
						let eventDataPtr = plusPtr blockPtr $ sizeOf (undefined :: RAWINPUTHEADER)
						eventType <- liftM f_RAWINPUTHEADER_dwType $ peek eventHeaderPtr
						case eventType of
							RIM_TYPEKEYBOARD -> do
								keyboardData <- peek $ castPtr eventDataPtr
								let key = keyFromVKKey $ f_RAWKEYBOARD_VKey keyboardData
								addEvent $ EventKeyboard $
									if (f_RAWKEYBOARD_Flags keyboardData .&. 1 {- RI_KEY_BREAK -}) == 0 then
										KeyDownEvent key
									else
										KeyUpEvent key
							RIM_TYPEMOUSE -> do
								mouseData <- peek $ castPtr eventDataPtr
								let flags = f_RAWMOUSE_usButtonFlags mouseData

								if (flags .&. 0x0001 {- RI_MOUSE_LEFT_BUTTON_DOWN -}) > 0 then
									addEvent $ EventMouse $ MouseDownEvent LeftMouseButton
								else if (flags .&. 0x0002 {- RI_MOUSE_LEFT_BUTTON_UP -}) > 0 then
									addEvent $ EventMouse $ MouseUpEvent LeftMouseButton
								else return ()

								if (flags .&. 0x0004 {- RI_MOUSE_RIGHT_BUTTON_DOWN -}) > 0 then
									addEvent $ EventMouse $ MouseDownEvent RightMouseButton
								else if (flags .&. 0x0008 {- RI_MOUSE_RIGHT_BUTTON_UP -}) > 0 then
									addEvent $ EventMouse $ MouseUpEvent RightMouseButton
								else return ()

								if (flags .&. 0x0010 {- RI_MOUSE_MIDDLE_BUTTON_DOWN -}) > 0 then
									addEvent $ EventMouse $ MouseDownEvent MiddleMouseButton
								else if (flags .&. 0x0020 {- RI_MOUSE_MIDDLE_BUTTON_UP -}) > 0 then
									addEvent $ EventMouse $ MouseUpEvent MiddleMouseButton
								else return ()

								let lastX = f_RAWMOUSE_lLastX mouseData
								let lastY = f_RAWMOUSE_lLastY mouseData
								let wheelChanged = (flags .&. 0x0400 {- RI_MOUSE_WHEEL -}) > 0

								if lastX /= 0 || lastY /= 0 || wheelChanged then do
									let wheel = if wheelChanged then f_RAWMOUSE_usButtonData mouseData else 0
									addEvent $ EventMouse $ RawMouseMoveEvent (fromIntegral lastX) (fromIntegral lastY) (fromIntegral wheel)
								else return ()

							_ -> return ()
					else return ()

			_ -> return ()

	-- register raw input
	success <- withArray
		[ RAWINPUTDEVICE
			{ f_RAWINPUTDEVICE_usUsagePage = 0x01
			, f_RAWINPUTDEVICE_usUsage = 0x02
			, f_RAWINPUTDEVICE_dwFlags = 0
			, f_RAWINPUTDEVICE_hwndTarget = hwnd
			}
		, RAWINPUTDEVICE
			{ f_RAWINPUTDEVICE_usUsagePage = 0x01
			, f_RAWINPUTDEVICE_usUsage = 0x06
			, f_RAWINPUTDEVICE_dwFlags = 0
			, f_RAWINPUTDEVICE_hwndTarget = hwnd
			}
		] $ \ridPtr -> do
		winapi_RegisterRawInputDevices ridPtr 2 (fromIntegral $ sizeOf (undefined :: RAWINPUTDEVICE))
	if not success then throwIO $ DescribeFirstException "failed to register raw input"
	else return ()

	return Win32InputManager
		{ mFrames = framesRef
		, mWindow = window
		}