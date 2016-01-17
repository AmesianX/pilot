//*@@@+++@@@@******************************************************************
//
// Microsoft Windows Media
// Copyright (C) Microsoft Corporation. All rights reserved.
//
//*@@@---@@@@******************************************************************
#include <stdio.h>
#include <stdarg.h>

#define INIT_MY_HEAP	0

#include "heaptrace.h"
#include "memtrace_com.h"

extern MemTrace_Context *g_memtrace_context;

/***********************************************************************
 *        Implementation of My_HeapAlloc and My_HeapFree.	           *
 ***********************************************************************/
EXTERN_C LPVOID My_HeapAlloc(
    IN HANDLE hHeap,
    IN DWORD dwFlags,
    IN SIZE_T dwBytes,
	char *pFile,
	int pLine
    )
{
	void *new_ptr = (*SysHeapAlloc)(hHeap, dwFlags, dwBytes);

#ifdef MULTI_THREAD_APP
	if (g_memtrace_context)	EnterCriticalSection(&g_memtrace_context->sync_section);
#endif

	RAM_TABLE *new_entry = register_ram_table(new_ptr, dwBytes);
	if (g_memtrace_context)
	{
		g_memtrace_context->g_heap_alloc_size += (int)dwBytes;
		RegAllocRamByFile(g_memtrace_context->g_heapfile_alloc_array, &g_memtrace_context->g_heapfile_alloc_counts, (int)dwBytes, pFile, pLine, new_entry, RAM_HEAP);
	}

#ifdef MULTI_THREAD_APP
	if (g_memtrace_context)	LeaveCriticalSection(&g_memtrace_context->sync_section);
#endif

	return new_ptr;
}

EXTERN_C BOOL My_HeapFree(
    IN HANDLE hHeap,
    IN DWORD dwFlags,
    IN LPVOID lpMem,
	char *pFile,
	int pLine
    )
{
#ifdef MULTI_THREAD_APP
	if (g_memtrace_context)	EnterCriticalSection(&g_memtrace_context->sync_section);
#endif

	if (g_memtrace_context)
	{
		RegFreeRamByFile(g_memtrace_context->g_heapfile_alloc_array, g_memtrace_context->g_heapfile_alloc_counts, lpMem);
		deregister_ram_table(lpMem);
	}

#ifdef MULTI_THREAD_APP
	if (g_memtrace_context)	LeaveCriticalSection(&g_memtrace_context->sync_section);
#endif

	return (*SysHeapFree)(hHeap, dwFlags, lpMem);
}

EXTERN_C void LogDebugStringMem(const _TCHAR *log_str, ...)
{
	_TCHAR buf[256];
	va_list marker;

	if (!log_str || _tcslen(log_str)>240)	return;

	va_start(marker, log_str);
	_vstprintf(buf, log_str, marker);
	va_end(marker);

	//OutputDebugString(buf);

	return;
}
