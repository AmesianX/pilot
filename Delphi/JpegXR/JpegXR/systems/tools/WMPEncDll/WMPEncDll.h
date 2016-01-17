// The following ifdef block is the standard way of creating macros which make exporting 
// from a DLL simpler. All files within this DLL are compiled with the WMPENCDLL_EXPORTS
// symbol defined on the command line. this symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see 
// WMPENCDLL_API functions as being imported from a DLL, whereas this DLL sees symbols
// defined with this macro as being exported.
#ifdef WMPENCDLL_EXPORTS
#define WMPENCDLL_API __declspec(dllexport) 
#else
#define WMPENCDLL_API __declspec(dllimport)
#endif

//extern WMPENCDLL_API int nWMPEncDll;

extern "C"
{
WMPENCDLL_API int BitmapStreamToXR(void* Data, int Size, int Quility, int PixelFormat, void** Output, int* OutputSize);
WMPENCDLL_API void FreeXR(void* Data);
}