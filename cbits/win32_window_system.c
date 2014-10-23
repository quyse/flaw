#include "flaw_windows.h"

const TCHAR wndClassName[] = TEXT("wc");

struct Win32WindowSystem
{

};

LRESULT WINAPI wndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	switch(msg)
	{
	default:
		return DefWindowProc(hWnd, msg, wParam, lParam);
	}
}

void initWin32WindowSystem(HANDLE* threadHandle)
{
	WNDCLASS wndClass;
	memset(&wndClass, 0, sizeof(wndClass));
	wndClass.lpfnWndProc = wndProc;
	wndClass.hInstance = GetModuleHandle(NULL);
	wndClass.hIcon = LoadIcon(NULL, IDI_APPLICATION);
	wndClass.hCursor = LoadCursor(NULL, IDC_ARROW);
	wndClass.hbrBackground = (HBRUSH)COLOR_WINDOW;
	wndClass.lpszClassName = wndClassName;
	RegisterClass(&wndClass);

	// get thread handle
	HANDLE currentThread = GetCurrentThread();
	DuplicateHandle(GetCurrentProcess(), currentThread, GetCurrentProcess(), threadHandle, THREAD_SET_CONTEXT, FALSE, 0);
}

void runWin32WindowSystem()
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

HWND createWin32Window(LPCTSTR title, int x, int y, int width, int height)
{
	return CreateWindowEx(
		0, // ex style
		wndClassName,
		title,
		WS_OVERLAPPEDWINDOW | WS_VISIBLE,
		x, y, width, height,
		NULL, // parent
		NULL, // menu
		GetModuleHandle(NULL), // hInstance
		NULL // lParam
		);
}

void setWin32WindowTitle(HWND hWnd, LPCTSTR title)
{
	SetWindowText(hWnd, title);
}

void stopWin32WindowSystem()
{
	PostQuitMessage(0);
}

void freeWin32WindowSystem(HANDLE thread)
{
	CloseHandle(thread);
}

// Haskell callback for invoke.
void hs_win32InvokeCallback(void* param);

void CALLBACK invokeWin32Callback(ULONG_PTR param)
{
	hs_win32InvokeCallback((void*)param);
}

void invokeWin32WindowSystem(HANDLE thread, void* param)
{
	QueueUserAPC(invokeWin32Callback, thread, (ULONG_PTR)param);
}
