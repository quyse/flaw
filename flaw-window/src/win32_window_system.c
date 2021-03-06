#define STRICT
#define WIN32_LEAN_AND_MEAN
#define UNICODE
#define _UNICODE
#define _WIN32_WINNT 0x0501
#include <windows.h>
#include <windowsx.h>
#include <winuser.h>
#include <stdlib.h>
#include <stdint.h>

const TCHAR wndClassName[] = TEXT("wc");

typedef void (*InvokeCallback)();

// must correspond to MouseCursor haskell enumeration.
const LPCTSTR cursorNames[] =
{
	IDC_ARROW,
	IDC_WAIT,
	IDC_APPSTARTING,
	IDC_IBEAM,
	IDC_SIZENWSE,
	IDC_SIZENESW,
	IDC_SIZEWE,
	IDC_SIZENS,
	IDC_SIZEALL,
	IDC_HAND
};

typedef struct
{
	HANDLE thread;
	DWORD threadId;
	HCURSOR cursors[sizeof(cursorNames) / sizeof(cursorNames[0])];
} Win32WindowSystem;

typedef void (*Win32WindowCallback)(UINT msg, WPARAM wParam, LPARAM lParam);

typedef struct
{
	Win32WindowSystem* windowSystem;
	HWND hWnd;
	HDC hdcLayered;
	HBITMAP bmpLayered;
	uint32_t* bmpLayeredData;
	Win32WindowCallback callback;
	int clientWidth;
	int clientHeight;
	BOOL active;
	HCURSOR cursor;
	// Needed to know because ShowCursor() works as a counter.
	BOOL cursorHidden;
	BOOL mouseLock;
} Win32Window;

Win32Window* initWin32Window(Win32WindowSystem* windowSystem, Win32WindowCallback callback)
{
	Win32Window* window = (Win32Window*)malloc(sizeof(Win32Window));
	window->windowSystem = windowSystem;
	window->hWnd = NULL;
	window->hdcLayered = NULL;
	window->bmpLayered = NULL;
	window->bmpLayeredData = NULL;
	window->callback = callback;
	window->clientWidth = 0;
	window->clientHeight = 0;
	window->active = TRUE;
	window->cursor = windowSystem->cursors[0];
	window->cursorHidden = FALSE;
	window->mouseLock = FALSE;
	return window;
}

void freeWin32Window(Win32Window* window)
{
	if(window->hdcLayered)
		DeleteDC(window->hdcLayered);
	if(window->bmpLayered)
		DeleteBitmap(window->bmpLayered);
}

Win32Window* getWin32Window(HWND hWnd)
{
	return (Win32Window*)GetWindowLongPtr(hWnd, GWLP_USERDATA);
}

void updateWin32WindowMouseLock(Win32Window* window);

LRESULT WINAPI win32WindowWndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	Win32Window* window = getWin32Window(hWnd);
	switch(msg)
	{
	case WM_CREATE:
		window = (Win32Window*)((CREATESTRUCT*)lParam)->lpCreateParams;
		SetWindowLongPtr(hWnd, GWLP_USERDATA, (LONG_PTR)window);
		break;
	case WM_ACTIVATE:
		if(window)
		{
			unsigned state = LOWORD(wParam);
			window->active = (state == WA_ACTIVE || state == WA_CLICKACTIVE);

			updateWin32WindowMouseLock(window);
		}
		break;
	case WM_MOVE:
		if(window)
			updateWin32WindowMouseLock(window);
		break;
	case WM_SIZE:
		if(window)
		{
			window->clientWidth = LOWORD(lParam);
			window->clientHeight = HIWORD(lParam);
			updateWin32WindowMouseLock(window);
		}
		break;
	case WM_DESTROY:
		SetWindowLongPtr(hWnd, GWLP_USERDATA, 0);
		freeWin32Window(window);
		break;
	case WM_SETCURSOR:
		if(LOWORD(lParam) == HTCLIENT)
		{
			SetCursor(window->cursor);
			return 0;
		}
		else
			return DefWindowProc(hWnd, msg, wParam, lParam);
	}

	// callback
	if(window && window->callback)
		window->callback(msg, wParam, lParam);

	switch(msg)
	{
	case WM_CREATE:
	case WM_SIZE:
	case WM_CLOSE:
	case WM_DESTROY:
		return 0;
	default:
		return DefWindowProc(hWnd, msg, wParam, lParam);
	}
}

Win32WindowSystem* initWin32WindowSystem()
{
	Win32WindowSystem* windowSystem = (Win32WindowSystem*)malloc(sizeof(Win32WindowSystem));

	WNDCLASS wndClass;
	memset(&wndClass, 0, sizeof(wndClass));
	wndClass.style = CS_OWNDC; // for OpenGL
	wndClass.lpfnWndProc = win32WindowWndProc;
	wndClass.hInstance = GetModuleHandle(NULL);
	wndClass.hIcon = LoadIcon(NULL, IDI_APPLICATION);
	wndClass.hCursor = LoadCursor(NULL, IDC_ARROW);
	wndClass.hbrBackground = GetStockBrush(BLACK_BRUSH);
	wndClass.lpszClassName = wndClassName;
	RegisterClass(&wndClass);

	// get thread handle
	HANDLE currentThread = GetCurrentThread();
	DuplicateHandle(GetCurrentProcess(), currentThread, GetCurrentProcess(), &windowSystem->thread, THREAD_SET_CONTEXT, FALSE, 0);
	// get thread id
	windowSystem->threadId = GetCurrentThreadId();

	// load cursors
	for(size_t i = 0; i < sizeof(cursorNames) / sizeof(cursorNames[i]); ++i)
	{
		windowSystem->cursors[i] = LoadCursor(NULL, cursorNames[i]);
	}

	return windowSystem;
}

void runWin32WindowSystem(Win32WindowSystem* windowSystem)
{
	MSG msg;

	for(;;)
	{
		DWORD result = MsgWaitForMultipleObjectsEx(0, NULL, INFINITE, QS_ALLEVENTS, MWMO_ALERTABLE | MWMO_INPUTAVAILABLE);
		// if there is some input
		if(result == WAIT_OBJECT_0)
		{
			while(PeekMessage(&msg, NULL, 0, 0, PM_REMOVE))
			{
				if(msg.message == WM_QUIT)
					return;
				TranslateMessage(&msg);
				DispatchMessage(&msg);
			}
		}
		// if there is APC action
		else if(result == WAIT_IO_COMPLETION)
		{
			// nothing to do
		}
		else
			// anything else is an error
			return;
	}
}

void setWin32WindowMouseCursor(HWND hWnd, int cursor)
{
	Win32Window* window = getWin32Window(hWnd);
	window->cursor = window->windowSystem->cursors[cursor];
}

void setWin32WindowMouseLock(HWND hWnd, int lock)
{
	Win32Window* window = getWin32Window(hWnd);
	window->mouseLock = !!lock;
	updateWin32WindowMouseLock(window);
}

void updateWin32WindowMouseLock(Win32Window* window)
{
	// get if we actually want to set mouse lock
	BOOL actualMouseLock = window->mouseLock && window->active;

	if(actualMouseLock)
	{
		// clip cursor into client rect in screen coordinates
		RECT rect;
		GetClientRect(window->hWnd, &rect);
		MapWindowPoints(window->hWnd, NULL, (LPPOINT)&rect, 2);
		ClipCursor(&rect);
	}
	else
		ClipCursor(NULL);

	if(actualMouseLock != window->cursorHidden)
	{
		ShowCursor(actualMouseLock ? FALSE : TRUE);
		window->cursorHidden = actualMouseLock;
	}
}

void createLayeredBitmap(Win32Window* window, HDC hdcScreen)
{
	// free old bitmap
	if(window->bmpLayered)
	{
		SelectBitmap(window->hdcLayered, NULL);
		DeleteBitmap(window->bmpLayered);
	}

	int clientWidth = window->clientWidth;
	int clientHeight = window->clientHeight;

	// create bitmap
	BITMAPINFOHEADER bh;
	ZeroMemory(&bh, sizeof(bh));
	bh.biSize = sizeof(bh);
	bh.biBitCount = 32;
	bh.biWidth = clientWidth;
	bh.biHeight = -clientHeight;
	bh.biPlanes = 1;

	window->bmpLayered = CreateDIBSection(hdcScreen, (const BITMAPINFO*)&bh, 0, (void**)&window->bmpLayeredData, NULL, 0);

	// select bitmap in hdc
	SelectBitmap(window->hdcLayered, window->bmpLayered);
}

HWND createWin32Window(Win32WindowSystem* windowSystem, LPCTSTR title, int x, int y, int width, int height, Win32WindowCallback callback, int layered)
{
	Win32Window* window = initWin32Window(windowSystem, callback);

	HWND hWnd = CreateWindowEx(
		layered ? WS_EX_LAYERED : 0, // ex style
		wndClassName,
		title,
		(layered ? WS_POPUP : WS_OVERLAPPEDWINDOW) | WS_VISIBLE,
		x, y, width, height,
		NULL, // parent
		NULL, // menu
		GetModuleHandle(NULL), // hInstance
		window // lParam
		);

	if(!hWnd)
		freeWin32Window(window);

	window->hWnd = hWnd;

	if(layered)
	{
		HDC hdcScreen = GetDC(NULL);

		// create DC
		window->hdcLayered = CreateCompatibleDC(hdcScreen);

		// create bitmap
		createLayeredBitmap(window, hdcScreen);

		ReleaseDC(NULL, hdcScreen);
	}

	return hWnd;
}

void updateLayeredWin32Window(HWND hWnd)
{
	Win32Window* window = getWin32Window(hWnd);
	HDC hdcScreen = GetDC(NULL);

	BLENDFUNCTION blendFunction;
	blendFunction.BlendOp = AC_SRC_OVER;
	blendFunction.BlendFlags = 0;
	blendFunction.SourceConstantAlpha = 255;
	blendFunction.AlphaFormat = AC_SRC_ALPHA;

	POINT position = { 0, 0 };
	POINT ptDest = { 0, 0 };
	SIZE szDest = { window->clientWidth, window->clientHeight };
	UpdateLayeredWindow(hWnd, hdcScreen, &ptDest, &szDest, window->hdcLayered, &position, 0, &blendFunction, ULW_ALPHA);

	ReleaseDC(NULL, hdcScreen);
}

void getLayeredWin32WindowBitmapData(HWND hWnd, void** bmpData, int* width, int* height, int* pitch)
{
	Win32Window* window = getWin32Window(hWnd);
	*bmpData = window->bmpLayeredData;
	*width = window->clientWidth;
	*height = window->clientHeight;
	*pitch = window->clientWidth * 4;
}

LPCWSTR getClipboardTextBegin(HWND hWnd, HANDLE* phMem, int* pLen)
{
	if(!OpenClipboard(hWnd))
		return NULL;
	*phMem = GetClipboardData(CF_UNICODETEXT);
	if(!*phMem)
	{
		CloseClipboard();
		return NULL;
	}
	LPCWSTR str = (LPCWSTR)GlobalLock(*phMem);
	if(!str)
	{
		CloseClipboard();
		return NULL;
	}
	*pLen = wcslen(str);
	return str;
}

void getClipboardTextEnd(HANDLE hMem)
{
	GlobalUnlock(hMem);
	CloseClipboard();
}

void setClipboardText(HWND hWnd, LPWSTR str)
{
	if(OpenClipboard(hWnd))
	{
		EmptyClipboard();
		size_t size = (wcslen(str) + 1) * sizeof(WCHAR);
		HANDLE hMem = GlobalAlloc(GMEM_MOVEABLE, size);
		if(hMem)
		{
			void* strCopy = GlobalLock(hMem);
			memcpy(strCopy, str, size);
			GlobalUnlock(hMem);
			SetClipboardData(CF_UNICODETEXT, hMem);
		}
		CloseClipboard();
	}
}

void setWin32WindowTitle(HWND hWnd, LPCTSTR title)
{
	SetWindowText(hWnd, title);
}

void getWin32WindowClientSize(HWND hWnd, int* width, int* height)
{
	Win32Window* window = getWin32Window(hWnd);
	*width = window->clientWidth;
	*height = window->clientHeight;
}

void destroyWin32Window(HWND hWnd)
{
	DestroyWindow(hWnd);
}

void stopWin32WindowSystem()
{
	PostQuitMessage(0);
}

void shutdownWin32WindowSystem(Win32WindowSystem* windowSystem)
{
	CloseHandle(windowSystem->thread);
	free(windowSystem);
}

void CALLBACK invokeWin32Callback(ULONG_PTR callback)
{
	((InvokeCallback)callback)();
}

void invokeWin32WindowSystem(Win32WindowSystem* windowSystem, void* callback)
{
	if(GetCurrentThreadId() == windowSystem->threadId)
		((InvokeCallback)callback)();
	else
		QueueUserAPC(invokeWin32Callback, windowSystem->thread, (ULONG_PTR)callback);
}
