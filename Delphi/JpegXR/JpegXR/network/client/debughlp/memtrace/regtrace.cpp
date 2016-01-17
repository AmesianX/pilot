//*@@@+++@@@@******************************************************************
//
// Microsoft Windows Media
// Copyright (C) Microsoft Corporation. All rights reserved.
//
//*@@@---@@@@******************************************************************
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
//#include <assert.h>

#define ENABLE_PTR 1
#include "regtrace.h"

typedef struct _REG_PTR_NODE
{
    void *ptr;
    int  id;
}REG_PTR_NODE;

#define MAX_REGPTR_ENTRIES  1000

REG_PTR_NODE    reg_ptr_table[MAX_REGPTR_ENTRIES];
int             reg_count = 0;

void InitRegTables()
{
    memset(reg_ptr_table, 0, sizeof(REG_PTR_NODE) * MAX_REGPTR_ENTRIES);
    reg_count = 0;
}

int RegPtr(void *ptr, int id)
{
    if (reg_count < MAX_REGPTR_ENTRIES && reg_ptr_table[reg_count].ptr == 0)
    {
        reg_ptr_table[reg_count].ptr = ptr;
        reg_ptr_table[reg_count].id = id;

        reg_count++;

        return (reg_count-1);
    }

    int i = reg_count;

    while(reg_count < MAX_REGPTR_ENTRIES)
    {
        if (reg_ptr_table[i].ptr == 0)
        {
            reg_ptr_table[i].ptr = ptr;
            reg_ptr_table[i].id = id;

            reg_count++;
            break;
        }

        i++;

        if (i>= MAX_REGPTR_ENTRIES)
            i = 0;

        //assert (i != reg_count); //means a full loop if equal happens, this must wrong.
    }

    return i;
}

void UnregPtr(void *ptr, int index)
{
    if (index>=0 && index<MAX_REGPTR_ENTRIES && reg_ptr_table[index].ptr == ptr)
    {
        reg_ptr_table[index].ptr = 0;
        reg_count--;
        return;
    }

    //Search for a matching ptr.
    for (int i=0; i<MAX_REGPTR_ENTRIES; i++)
        if (reg_ptr_table[i].ptr == ptr)
        {
            reg_ptr_table[i].ptr = 0;
            reg_count--;
            return;
        }

    fprintf(stderr, "Error to Unregister pointer %p.\n", ptr);
}

void ReportLivingPtr(char *file_name)
{
    FILE *report_file;

    if (file_name)
    {
        report_file = fopen(file_name, "a");
    }
    else
        report_file = stdout;

    fprintf(report_file, "\n*************************************************\n");
    fprintf(report_file, "Living Pointers List.\n");
    fprintf(report_file, "*************************************************\n");
    for (int i=0; i<MAX_REGPTR_ENTRIES; i++)
    {
        if (reg_ptr_table[i].ptr != 0)
            fprintf(report_file, "Pointer: %p, Id = %d.\n", reg_ptr_table[i].ptr, reg_ptr_table[i].id);
    }

    if (report_file!=stdout)
        fclose(report_file);
}
