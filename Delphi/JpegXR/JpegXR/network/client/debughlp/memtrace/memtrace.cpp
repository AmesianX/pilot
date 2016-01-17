//*@@@+++@@@@******************************************************************
//
// Microsoft Windows Media
// Copyright (C) Microsoft Corporation. All rights reserved.
//
//*@@@---@@@@******************************************************************
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <malloc.h>
#include <math.h>

//#define TRACE_NEW		1
//#define TRACE_MALLOC	1
//#define TRACE_STACK		1

#include "memtrace.h"

#include <TCHAR.h>
EXTERN_C void LogDebugStringMem(const _TCHAR *log_str, ...);

EXTERN_C int WriteReportLogs(const char *report_file, char *log_format, ...)
{
	FILE *fd = fopen(report_file, "at");
	if (!fd)	return 0;

	va_list args;
	va_start(args, log_format);
	vfprintf(fd, log_format, args);
	va_end(args);
	fclose(fd);

	return 1;
}


/**************************************************************
 *     Trace heap, new, malloc functions					  *
 **************************************************************/

#define PEEK_TRACE_INIT_LEVEL	200000	//200K
#define PEEK_TRACE_STEP			50000	//50K
#define MAX_RAMS	5000


MemTrace_Context *g_memtrace_context = 0;

EXTERN_C MemTrace_Context *Create_MemTraceContext()
{
	MemTrace_Context *context = (MemTrace_Context *) SysMalloc(sizeof(MemTrace_Context));
	context->g_malloc_size=0;
	context->g_dyn_malloc_size=0;	//dyn alloc memory size
	context->g_dyn_max = 0;			//maxium dyn malloc memory size
	context->g_heap_alloc_size=0;	//total heap allocated memory size
	context->g_malloc_alloc_size=0;	//total malloc allocated memory size
	context->g_new_alloc_size=0;		//total new allocated memory size
	context->g_free_size = 0;		//total freed memory
	context->g_last_mark_file = 0;
	context->g_last_line_no = 0;
	context->g_report_file = 0;

	context->g_heapfile_alloc_array = (RAM_FILE_UNIT *)SysMalloc(sizeof(RAM_FILE_UNIT) * MAX_FILES_ALLOC);
	memset(context->g_heapfile_alloc_array, 0, sizeof(RAM_FILE_UNIT) * MAX_FILES_ALLOC);
	context->g_heapfile_alloc_counts=0;
	context->g_old_peek_size = 0;
	context->g_ram_table = (RAM_TABLE *)SysMalloc(sizeof(RAM_TABLE) * MAX_RAMS);
	memset(context->g_ram_table, 0, sizeof(RAM_TABLE) * MAX_RAMS);

#ifdef MULTI_THREAD_APP
	context->sync_section = InitializeCriticalSection(&sync_section);
#endif
	return context;
}


EXTERN_C RAM_TABLE*  register_ram_table(void *new_ptr, size_t n)
{
	if (!new_ptr || !g_memtrace_context)	return 0;

	g_memtrace_context->g_malloc_size += (int)n;
	g_memtrace_context->g_dyn_malloc_size += (int)n;
	g_memtrace_context->g_dyn_max= (g_memtrace_context->g_dyn_malloc_size > g_memtrace_context->g_dyn_max) ? g_memtrace_context->g_dyn_malloc_size : g_memtrace_context->g_dyn_max;

	if ( ( abs(g_memtrace_context->g_dyn_max - g_memtrace_context->g_old_peek_size) > PEEK_TRACE_STEP ) && ( g_memtrace_context->g_dyn_max > PEEK_TRACE_INIT_LEVEL ) )
	{
		g_memtrace_context->g_old_peek_size = g_memtrace_context->g_dyn_max;

		FILE *fd = fopen(g_memtrace_context->g_report_file, "at");
		if (!fd)	return false;

		fprintf(fd, "\n*********************\n");
		fprintf(fd, "Peek Ram Used: %d\n Peek Ram Distributions:\n", g_memtrace_context->g_dyn_max);
		//LogDebugStringMem(_T("Peek Ram Used: %d\n Peek Ram Distributions:\n"), g_memtrace_context->g_dyn_max);
		for (int i=0; i<g_memtrace_context->g_heapfile_alloc_counts; i++)
		{
			if (g_memtrace_context->g_heapfile_alloc_array[i].dyn_ram_bytes > 0)
			{
				fprintf(fd, "%d bytes(%f percent) in %s line %d.\n", g_memtrace_context->g_heapfile_alloc_array[i].dyn_ram_bytes, (float)g_memtrace_context->g_heapfile_alloc_array[i].dyn_ram_bytes / g_memtrace_context->g_dyn_max, g_memtrace_context->g_heapfile_alloc_array[i].filename, g_memtrace_context->g_heapfile_alloc_array[i].line_no);
				//LogDebugStringMem(_T("%d bytes(%f percent) in %s line %d.\n"), g_memtrace_context->g_heapfile_alloc_array[i].dyn_ram_bytes, (float)g_memtrace_context->g_heapfile_alloc_array[i].dyn_ram_bytes / g_memtrace_context->g_dyn_max, _TEXT(g_memtrace_context->g_heapfile_alloc_array[i].filename), g_memtrace_context->g_heapfile_alloc_array[i].line_no);
			}
		}
		fclose(fd);
	}

	for (int i=0; i<MAX_RAMS; i++)
	{
		if (g_memtrace_context->g_ram_table[i].ptr==0)
		{
			g_memtrace_context->g_ram_table[i].ram_unit=0;
			g_memtrace_context->g_ram_table[i].ptr=new_ptr;
			g_memtrace_context->g_ram_table[i].size=n;
			return &g_memtrace_context->g_ram_table[i];
		}
	}
	return 0;
}

EXTERN_C void deregister_ram_table(void *new_ptr)
{
	if (!new_ptr || !g_memtrace_context)	return;

	for (int i=0; i<MAX_RAMS; i++)
	{
		if (g_memtrace_context->g_ram_table[i].ptr==new_ptr)
		{
			g_memtrace_context->g_dyn_malloc_size -= (int)g_memtrace_context->g_ram_table[i].size;
			g_memtrace_context->g_ram_table[i].ptr=0;
			g_memtrace_context->g_ram_table[i].ram_unit=0;
			g_memtrace_context->g_free_size += (int)g_memtrace_context->g_ram_table[i].size;
			break;
		}
	}
}

EXTERN_C void RegAllocRamByFile(RAM_FILE_UNIT *pfile_alloc_array, int *pfile_alloc_counts, int pBytes, char *pFile, int line_no, RAM_TABLE *pRamTable, int pRamType)
{
    int i;
	for (i=0; i<*pfile_alloc_counts; i++)
	{
		if (!strcmp(pfile_alloc_array[i].filename, pFile) && pfile_alloc_array[i].line_no==line_no)
		{
			pfile_alloc_array[i].ram_bytes+=pBytes;
			pfile_alloc_array[i].dyn_ram_bytes+=pBytes;
			if (pRamTable){
				pRamTable->ram_unit = &pfile_alloc_array[i];
				pRamTable = 0;
			}

			return;
		}
	}

	if (i<MAX_FILES_ALLOC)
	{
		strcpy(pfile_alloc_array[i].filename, pFile);
		pfile_alloc_array[i].line_no = line_no;
		pfile_alloc_array[i].ram_bytes = pBytes;
		pfile_alloc_array[i].dyn_ram_bytes=pBytes;
		pfile_alloc_array[i].ram_type=pRamType;

		if (pRamTable)
		{
			pRamTable->ram_unit = &pfile_alloc_array[i];
			pRamTable = 0;
		}

		*pfile_alloc_counts = i+1;
	}
}

EXTERN_C void RegFreeRamByFile(RAM_FILE_UNIT *pfile_alloc_array, int pfile_alloc_counts, void *pMem)
{
	if (!pMem || !g_memtrace_context)	return;

	for (int i=0; i<MAX_RAMS; i++)
	{
		if (g_memtrace_context->g_ram_table[i].ptr==pMem && g_memtrace_context->g_ram_table[i].ram_unit)
		{
			g_memtrace_context->g_ram_table[i].ram_unit->dyn_ram_bytes -= (int)g_memtrace_context->g_ram_table[i].size;
			return;
		}
	}
}

/***********************************************************************
 *        Implementation of my_malloc and my_free                      *
 ***********************************************************************/

EXTERN_C void * my_malloc(size_t pSize, char *pFile, int pLine)
{
	void *new_ptr = SysMalloc(pSize);

#ifdef MULTI_THREAD_APP
	if (g_memtrace_context)	EnterCriticalSection(&g_memtrace_context->sync_section);
#endif

	RAM_TABLE *new_entry = register_ram_table(new_ptr, pSize);

	if (g_memtrace_context)
	{
		g_memtrace_context->g_malloc_alloc_size += (int)pSize;
		RegAllocRamByFile(g_memtrace_context->g_heapfile_alloc_array, &g_memtrace_context->g_heapfile_alloc_counts, (int)pSize, pFile, pLine, new_entry, RAM_MALLOC);
	}

#ifdef MULTI_THREAD_APP
	if (g_memtrace_context)	LeaveCriticalSection(&g_memtrace_context->sync_section);
#endif

	return new_ptr;
}

EXTERN_C void my_free(void *lpMem, char *pFile, int pLine)
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

	SysFree(lpMem);
}


/***********************************************************************
 *        Implementation of my_new and my_delete                      *
 ***********************************************************************/
EXTERN_C void * my_new( unsigned int pSize )
{
    void *new_ptr = malloc(pSize );

#ifdef MULTI_THREAD_APP
	if (g_memtrace_context)	EnterCriticalSection(&g_memtrace_context->sync_section);
#endif

	if (g_memtrace_context)
	{
		if (g_memtrace_context->g_last_mark_file)
		{
            RAM_TABLE *new_entry = register_ram_table(new_ptr, pSize);

			RegAllocRamByFile(g_memtrace_context->g_heapfile_alloc_array, &g_memtrace_context->g_heapfile_alloc_counts, pSize, g_memtrace_context->g_last_mark_file, g_memtrace_context->g_last_line_no, new_entry, RAM_NEW);
			g_memtrace_context->g_last_mark_file = 0;

            g_memtrace_context->g_new_alloc_size += pSize;
		}

	}

#ifdef MULTI_THREAD_APP
	if (g_memtrace_context)	LeaveCriticalSection(&g_memtrace_context->sync_section);
#endif

    return new_ptr;
}

EXTERN_C void my_delete(void *lpMem)
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

	free(lpMem);
}

EXTERN_C void Mark_This_Position(char *file, int line_no)
{
	if (g_memtrace_context)
	{
		g_memtrace_context->g_last_mark_file = file;
		g_memtrace_context->g_last_line_no = line_no;
	}
}

/////////////////////////////////////////////////////////////////////////////
EXTERN_C void Heap_Measure_Start(const char *logfile)
{
	if (g_memtrace_context)
	{
		SysFree(g_memtrace_context);
	}
	g_memtrace_context = Create_MemTraceContext();
	g_memtrace_context->g_report_file = logfile;
}

EXTERN_C int WriteMemTraceLog()
{
	if (!g_memtrace_context || !g_memtrace_context->g_report_file)	return 0;

	FILE *fd = fopen(g_memtrace_context->g_report_file, "at");
	if (!fd)	return 0;

	fprintf(fd, "\nMemory Statistics:\n");
	fprintf(fd, "Allocated Total Memory: %d bytes.\n", g_memtrace_context->g_malloc_size);
	fprintf(fd, "Allocated Free Memory: %d bytes.\n", g_memtrace_context->g_free_size);
	fprintf(fd, "HeapAlloc Memory: %d bytes.\n", g_memtrace_context->g_heap_alloc_size);
	fprintf(fd, "Malloc Memory: %d bytes.\n", g_memtrace_context->g_malloc_alloc_size);
	fprintf(fd, "New Memory: %d bytes.\n", g_memtrace_context->g_new_alloc_size);
	fprintf(fd, "Maximum Ram Used: %d bytes.\n", g_memtrace_context->g_dyn_max);

	fprintf(fd, "Detailed Heap Allocated.\n");
    int i;
	for(i=0; i<g_memtrace_context->g_heapfile_alloc_counts; i++)
	{
		if (g_memtrace_context->g_heapfile_alloc_array[i].ram_type == RAM_HEAP)
			fprintf(fd, "\n%d bytes in %s %d line. Remain %d bytes.", g_memtrace_context->g_heapfile_alloc_array[i].ram_bytes, g_memtrace_context->g_heapfile_alloc_array[i].filename, g_memtrace_context->g_heapfile_alloc_array[i].line_no, g_memtrace_context->g_heapfile_alloc_array[i].dyn_ram_bytes);
	}

	fprintf(fd, "\n\nDetailed New Allocated.\n");
	for(i=0; i<g_memtrace_context->g_heapfile_alloc_counts; i++)
	{
		if (g_memtrace_context->g_heapfile_alloc_array[i].ram_type == RAM_NEW)
			fprintf(fd, "\n%d bytes in %s %d line. Remain %d bytes.", g_memtrace_context->g_heapfile_alloc_array[i].ram_bytes, g_memtrace_context->g_heapfile_alloc_array[i].filename, g_memtrace_context->g_heapfile_alloc_array[i].line_no, g_memtrace_context->g_heapfile_alloc_array[i].dyn_ram_bytes);
	}

	fprintf(fd, "\n\nDetailed Malloc Allocated.\n");
	for(i=0; i<g_memtrace_context->g_heapfile_alloc_counts; i++)
	{
		if (g_memtrace_context->g_heapfile_alloc_array[i].ram_type == RAM_MALLOC)
			fprintf(fd, "\n%d bytes in %s %d line. Remain %d bytes.", g_memtrace_context->g_heapfile_alloc_array[i].ram_bytes, g_memtrace_context->g_heapfile_alloc_array[i].filename, g_memtrace_context->g_heapfile_alloc_array[i].line_no, g_memtrace_context->g_heapfile_alloc_array[i].dyn_ram_bytes);
	}

	fprintf(fd, "\n\n");

	fclose(fd);

#ifdef MULTI_THREAD_APP
	DeleteCriticalSection( &g_memtrace_context->sync_section );
#endif

	SysFree(g_memtrace_context);
	g_memtrace_context = 0;
	return 1;
}

//Measure the depth of using stack
#define MAX_STACK_NO	4
volatile int* g_piStackMeasureBase[MAX_STACK_NO]={0, 0, 0, 0};
EXTERN_C void StackMeasureStart(int stack_no)
{
    volatile int stackinit[STACK_MEASURE_TOPS];
    int i;
	if (stack_no < MAX_STACK_NO && stack_no>=0 )
		g_piStackMeasureBase[stack_no] = stackinit;

    for( i=0; i<STACK_MEASURE_TOPS; i++ )
        stackinit[i] = 0x13579BDF;
}    

EXTERN_C int cbStackUsed(int stack_no)
{
    //volatile int stackinit[STACK_MEASURE_TOPS];
    int i;
    volatile int *stackinit = g_piStackMeasureBase[stack_no];

//        assert( abs(g_piStackMeasureBase[stack_no] - stackinit) <= 16);

//    if (abs(g_piStackMeasureBase[stack_no] - stackinit) > 16 )
//        return -1;

#if 1
    // stack grows down
    for( i=0; i<STACK_MEASURE_TOPS; i++ )
    {
        if ( stackinit[i] != 0x13579BDF )
            break;
    }
    return (STACK_MEASURE_TOPS-i)*sizeof(int);

#else
    // stack grows up
    for( i=STACK_MEASURE_TOPS-1; i>=0; i-- )
    {
        if ( stackinit[i] != 0x13579BDF )
            break;
    }
    return i*sizeof(int);

#endif
}

