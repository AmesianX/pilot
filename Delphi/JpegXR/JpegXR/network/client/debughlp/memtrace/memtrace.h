//*@@@+++@@@@******************************************************************
//
// Microsoft Windows Media
// Copyright (C) Microsoft Corporation. All rights reserved.
//
//*@@@---@@@@******************************************************************
typedef void *(MallocType)(size_t pSize);
typedef void (FreeType)(void *pMem);

extern MallocType	*SysMalloc;
extern FreeType		*SysFree;

#ifndef MEM_TRACE_DEBUG
#define MEM_TRACE_DEBUG

#include "memtrace_com.h"

EXTERN_C int WriteReportLogs(const char *report_file, char *log_format, ...);

#define WRITE_MEM_REPORT(fd, pheapfile_alloc_array, pheapfile_alloc_counts) \
{\
	for (int i=0; i<pheapfile_alloc_counts; i++) \
	{ \
		fprintf(fd, "\n%d bytes in %s %d line. Remain %d bytes.", pheapfile_alloc_array[i].ram_bytes, pheapfile_alloc_array[i].filename, pheapfile_alloc_array[i].line_no, pheapfile_alloc_array[i].dyn_ram_bytes); \
	}\
}

#define STACK_MEASURE_TOPS 6000
	
#if TRACE_STACK

EXTERN_C void StackMeasureStart(int);

EXTERN_C int cbStackUsed(int);
#   define STACK_MEASURE_START(stack_no) StackMeasureStart(stack_no);
#   define STACK_MEASURE_STOP(a, stack_no) int a = cbStackUsed(stack_no);

#else  // TRACE_STACK

#   define STACK_MEASURE_START(stack_no)
#   define STACK_MEASURE_STOP(a, stack_no)

#endif // TRACE_STACK

/************************************************************************
 * The following definition help to measure new allocated memory.       *
 ************************************************************************/

#if TRACE_NEW

EXTERN_C void * my_new( unsigned int pSize );
EXTERN_C void my_delete(void *lpMem);

inline void * __cdecl operator new( unsigned int pSize )
{
	return my_new(pSize);
}

inline void __cdecl operator delete(void *lpMem)
{
	my_delete(lpMem);
}

EXTERN_C void Mark_This_Position(char *file, int line_no);

#define MARK_THIS_POSITION	Mark_This_Position(__FILE__, __LINE__);

#endif //TRACE_NEW


/************************************************************************
 * Definition to overide Malloc, Free, used for heap statistics. *
 ************************************************************************/
#if TRACE_MALLOC

EXTERN_C void * my_malloc(size_t pSize, char *pFile, int pLine);
EXTERN_C void   my_free(void *pMem, char *pFile, int pLine);

#define malloc(dwBytes) \
	my_malloc(dwBytes, __FILE__, __LINE__)

#define free(lpMem)	\
	my_free(lpMem, __FILE__, __LINE__)


#endif //TRACE_MALLOC

#if TRACE_HEAP
#include "heaptrace.h"
#endif //TRACE_HEAP

#if (TRACE_NEW || TRACE_MALLOC || TRACE_HEAP)

EXTERN_C int WriteMemTraceLog();
EXTERN_C void Heap_Measure_Start(const char *log_file);

#define WRITE_MEM_LOG()					WriteMemTraceLog();
#define MEM_MEASURE_START(log_file)	    Heap_Measure_Start(log_file);

#else

#define MEM_MEASURE_START(log_file)
#define MARK_THIS_POSITION
#define WRITE_MEM_LOG()

#endif //(TRACE_NEW || TRACE_MALLOC || TRACE_HEAP)

#endif //MEM_TRACE_DEBUG
