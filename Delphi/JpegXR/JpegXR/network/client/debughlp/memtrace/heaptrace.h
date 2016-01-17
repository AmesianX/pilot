//*@@@+++@@@@******************************************************************
//
// Microsoft Windows Media
// Copyright (C) Microsoft Corporation. All rights reserved.
//
//*@@@---@@@@******************************************************************
#ifndef _HEAP_TRACE_DEBUG
#define _HEAP_TRACE_DEBUG

#ifndef UNDER_CE
// qunlili - do not include winbase.h for VS2005 WinCE platform as it creates macro redefinition for HeapAlloc
#include <windows.h>
#include <winbase.h>
#endif
#include <TCHAR.H>

#ifndef EXTERN_C

#ifdef __cplusplus
#define EXTERN_C extern "C"
#else
#define EXTERN_C
#endif

#endif

#if INIT_MY_HEAP

typedef LPVOID (HeapAllocType)(IN HANDLE hHeap, IN DWORD dwFlags, IN SIZE_T dwBytes);
typedef BOOL (HeapFreeType)(IN HANDLE hHeap, IN DWORD dwFlags, IN LPVOID lpMem);
HeapAllocType	* SysHeapAlloc = HeapAlloc;
HeapFreeType	* SysHeapFree = HeapFree;

#else

typedef LPVOID (HeapAllocType)(IN HANDLE hHeap, IN DWORD dwFlags, IN SIZE_T dwBytes);
typedef BOOL (HeapFreeType)(IN HANDLE hHeap, IN DWORD dwFlags, IN LPVOID lpMem);
extern HeapAllocType	* SysHeapAlloc;
extern HeapFreeType	* SysHeapFree;

#endif //INIT_MY_HEAP

EXTERN_C void LogDebugStringMem(const _TCHAR *log_str, ...);


#define DEFINE_PROFILE_COUNTER(counter) \
	LARGE_INTEGER counter;

	
#define INIT_PROFILE_STAMP(counter) \
	memset(&counter, 0, sizeof(counter));


#define START_PROFILE_STAMP \
	LARGE_INTEGER base_stamp_l; \
	if (!QueryPerformanceCounter(&base_stamp_l)) \
		memset(&base_stamp_l, 0, sizeof(base_stamp_l));


#define STOP_PROFILE_STAMP(counter) \
	LARGE_INTEGER stop_stamp_l; \
	if (QueryPerformanceCounter(&stop_stamp_l)) \
	{ \
		counter.QuadPart += stop_stamp_l.QuadPart - base_stamp_l.QuadPart; \
		LogDebugStringMem(L"Profile Counter:%d%d\n", counter.HighPart, counter.LowPart); \
	}


#define END_PROFILE_STAMP(counter, secs) \
		float secs = 0; \
        LARGE_INTEGER liPerformanceFrequency; \
        if( QueryPerformanceFrequency( &liPerformanceFrequency ) ) \
		{ \
			secs = (float)counter.QuadPart / liPerformanceFrequency.QuadPart; \
		}


/************************************************************************
 * Definition to overide HeapAlloc, HeapFree, used for heap statistics. *
 ************************************************************************/

EXTERN_C LPVOID My_HeapAlloc(
    IN HANDLE hHeap,
    IN DWORD dwFlags,
    IN SIZE_T dwBytes,
	char *pFile,
	int pLine
    );

EXTERN_C BOOL My_HeapFree(
    IN HANDLE hHeap,
    IN DWORD dwFlags,
    IN LPVOID lpMem,
	char *pFile,
	int pLine
    );

#define HeapAlloc(hHeap, dwFlags, dwBytes) \
	My_HeapAlloc(hHeap, dwFlags, dwBytes, __FILE__, __LINE__)
	
#define HeapFree(hHeap, dwFlags, lpMem)	\
	My_HeapFree(hHeap, dwFlags, lpMem, __FILE__, __LINE__)

#endif //_HEAP_TRACE_DEBUG
