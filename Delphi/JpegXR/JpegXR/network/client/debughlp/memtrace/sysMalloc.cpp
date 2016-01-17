//*@@@+++@@@@******************************************************************
//
// Microsoft Windows Media
// Copyright (C) Microsoft Corporation. All rights reserved.
//
//*@@@---@@@@******************************************************************
#include <stdio.h>
#include <stdlib.h>

typedef void *(MallocType)(size_t pSize);
typedef void (FreeType)(void *pMem);

extern "C" void* stdMalloc(size_t pSize)
{
    return malloc(pSize);
}

extern "C" void stdFree(void *pMem)
{
    free(pMem);
}

MallocType	*SysMalloc = stdMalloc;
FreeType	*SysFree = stdFree;

