//*@@@+++@@@@******************************************************************
//
// Microsoft Windows Media
// Copyright (C) Microsoft Corporation. All rights reserved.
//
//*@@@---@@@@******************************************************************
#ifndef _MEMTRACE_COMMON_STRUCT_
#define _MEMTRACE_COMMON_STRUCT_

#ifdef MULTI_THREAD_APP
#include <windows.h>
#endif

#ifndef EXTERN_C

#ifdef __cplusplus
#define EXTERN_C extern "C"
#else
#define EXTERN_C
#endif

#endif //EXTERN_C

#define RAM_HEAP	0
#define RAM_NEW		1
#define RAM_MALLOC	2

typedef struct _RAM_FILE_UNIT //Record the usage of allocation by filename and line_no
{
	char filename[256];
	int	 ram_type;			//HeapAlloc, New, Malloc
	int  line_no;
	int  ram_bytes;			//total ram allocated
	int  dyn_ram_bytes;		//Removed from HeapFree.
}RAM_FILE_UNIT;

typedef struct _RAM_TABLE
{
	void   *ptr;
	size_t  size;
	RAM_FILE_UNIT	*ram_unit;
}RAM_TABLE;					//Registered Entry for every allocated memory.

typedef struct{
	int g_malloc_size;		//total alloc memory size
	int g_dyn_malloc_size;	//dyn alloc memory size
	int g_dyn_max;			//maxium dyn malloc memory size
	int g_heap_alloc_size;	//total heap allocated memory size
	int g_malloc_alloc_size;	//total malloc allocated memory size
	int g_new_alloc_size;		//total new allocated memory size
	int g_free_size;		//total freed memory
	char* g_last_mark_file;
	int   g_last_line_no;
	const char *g_report_file;
	int g_old_peek_size;
	RAM_TABLE *g_ram_table;

	RAM_FILE_UNIT *g_heapfile_alloc_array;
	int			  g_heapfile_alloc_counts;

#ifdef MULTI_THREAD_APP
	CRITICAL_SECTION 	sync_section;
#endif

}MemTrace_Context;

#define MAX_FILES_ALLOC	200  //maximum file allocations for alloc mem.

EXTERN_C RAM_TABLE*  register_ram_table(void *new_ptr, size_t n);
EXTERN_C void deregister_ram_table(void *new_ptr);
EXTERN_C void RegAllocRamByFile(RAM_FILE_UNIT *pfile_alloc_array, int *pfile_alloc_counts, int pBytes, char *pFile, int line_no, RAM_TABLE *pRamTable, int pRamType);
EXTERN_C void RegFreeRamByFile(RAM_FILE_UNIT *pfile_alloc_array, int pfile_alloc_counts, void *pMem);

#endif
