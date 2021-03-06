#define STRICT
#define WIN32_LEAN_AND_MEAN
#define UNICODE
#define _UNICODE
#define _WIN32_WINNT 0x0501
#include <windows.h>
#include <windowsx.h>

typedef HGLRC (WINAPI *PFNWGLCREATECONTEXTATTRIBSARBPROC)(HDC hDC, HGLRC hShareContext, const int* attribList);

#define WGL_CONTEXT_MAJOR_VERSION_ARB 0x2091
#define WGL_CONTEXT_MINOR_VERSION_ARB 0x2092
#define WGL_CONTEXT_FLAGS_ARB 0x2094
#define WGL_CONTEXT_PROFILE_MASK_ARB 0x9126
#define WGL_CONTEXT_DEBUG_BIT_ARB 0x0001
#define WGL_CONTEXT_CORE_PROFILE_BIT_ARB 0x00000001

HDC initWin32OpenGLContext(HWND hWnd, int debug, HGLRC* phglrcMain, HGLRC* phglrcBackground)
{
	// get window's persistent HDC
	HDC hdc = GetDC(hWnd);
	if(!hdc) return NULL;

	// choose & set pixel format
	PIXELFORMATDESCRIPTOR pfd;
	ZeroMemory(&pfd, sizeof(pfd));
	pfd.nSize = sizeof(pfd);
	pfd.nVersion = 1;
	pfd.dwFlags = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER;
	pfd.iPixelType = PFD_TYPE_RGBA;
	pfd.cColorBits = 24;
	pfd.cDepthBits = 0;
	pfd.iLayerType = PFD_MAIN_PLANE;
	int pixelFormat = ChoosePixelFormat(hdc, &pfd);
	if(!pixelFormat) return NULL;
	if(!SetPixelFormat(hdc, pixelFormat, &pfd)) return NULL;

	// create temporary context and make it current
	HGLRC hglrcTemp = wglCreateContext(hdc);
	if(!wglMakeCurrent(hdc, hglrcTemp)) return NULL;

	// create real context
	int attribs[] =
	{
		WGL_CONTEXT_MAJOR_VERSION_ARB, 0,
		WGL_CONTEXT_MINOR_VERSION_ARB, 0,
		WGL_CONTEXT_FLAGS_ARB, debug ? WGL_CONTEXT_DEBUG_BIT_ARB : 0,
		WGL_CONTEXT_PROFILE_MASK_ARB, WGL_CONTEXT_CORE_PROFILE_BIT_ARB,
		0, 0
	};
	// versions to try
	static const int versions[][2] =
	{
		{ 4, 3 },
		{ 4, 2 },
		{ 4, 1 },
		{ 4, 0 },
		{ 3, 3 },
	};
	PFNWGLCREATECONTEXTATTRIBSARBPROC wglCreateContextAttribsARB = (PFNWGLCREATECONTEXTATTRIBSARBPROC)wglGetProcAddress("wglCreateContextAttribsARB");
	if(!wglCreateContextAttribsARB)
	{
		wglMakeCurrent(hdc, NULL);
		wglDeleteContext(hglrcTemp);
		return NULL;
	}
	// loop for versions
	HGLRC hglrcMain = NULL, hglrcBackground = NULL;
	for(int i = 0; i < sizeof(versions) / sizeof(versions[0]); ++i)
	{
		attribs[1] = versions[i][0];
		attribs[3] = versions[i][1];
		hglrcMain = wglCreateContextAttribsARB(hdc, 0, attribs);
		hglrcBackground = wglCreateContextAttribsARB(hdc, 0, attribs);
		if(hglrcMain && hglrcBackground) break;
		if(hglrcMain) wglDeleteContext(hglrcMain);
		if(hglrcBackground) wglDeleteContext(hglrcBackground);
	}

	// delete temporary context
	wglMakeCurrent(hdc, NULL);
	wglDeleteContext(hglrcTemp);

	// if no version is supported, bummer
	if(!hglrcMain || !hglrcBackground) return NULL;

	// make contexts share resources
	if(!wglShareLists(hglrcMain, hglrcBackground))
	{
		wglDeleteContext(hglrcMain);
		wglDeleteContext(hglrcBackground);
		return NULL;
	}

	*phglrcMain = hglrcMain;
	*phglrcBackground = hglrcBackground;
	return hdc;
}

int setCurrentWin32OpenGLContext(HDC hdc, HGLRC hglrc)
{
	return wglMakeCurrent(hdc, hglrc);
}

void deinitWin32OpenGLContext(HWND hWnd, HGLRC hglrc)
{
	wglMakeCurrent(GetDC(hWnd), hglrc);
	wglDeleteContext(hglrc);
}

void swapWin32OpenGLWindow(HDC hdc)
{
	SwapBuffers(hdc);
}
