//*@@@+++@@@@******************************************************************
//
// Microsoft Windows Media
// Copyright (C) Microsoft Corporation. All rights reserved.
//
//*@@@---@@@@******************************************************************
#include <windows.h>

typedef LPVOID (HeapAllocType)(IN HANDLE hHeap, IN DWORD dwFlags, IN SIZE_T dwBytes);
typedef BOOL (HeapFreeType)(IN HANDLE hHeap, IN DWORD dwFlags, IN LPVOID lpMem);

extern "C" LPVOID stdHeapAlloc(IN HANDLE hHeap, IN DWORD dwFlags, IN SIZE_T dwBytes)
{
    return HeapAlloc(hHeap, dwFlags, dwBytes);
}

extern "C" BOOL stdHeapFree(IN HANDLE hHeap, IN DWORD dwFlags, IN LPVOID lpMem)
{
    return HeapFree(hHeap, dwFlags, lpMem);
}

HeapAllocType	* SysHeapAlloc = stdHeapAlloc;
HeapFreeType	* SysHeapFree = stdHeapFree;

