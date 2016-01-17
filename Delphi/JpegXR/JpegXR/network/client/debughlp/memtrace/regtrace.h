//*@@@+++@@@@******************************************************************
//
// Microsoft Windows Media
// Copyright (C) Microsoft Corporation. All rights reserved.
//
//*@@@---@@@@******************************************************************
#ifndef H_REGTRACE_H
#define H_REGTRACE_H

#ifndef EXTERN_C

#ifdef __cplusplus
#define EXTERN_C extern "C"
#else
#define EXTERN_C
#endif

#endif //EXTERN_C

#if ENABLE_PTR

EXTERN_C void InitRegTables();
EXTERN_C int RegPtr(void *ptr, int id = 0);
EXTERN_C void UnregPtr(void *ptr, int index = -1);
EXTERN_C void ReportLivingPtr(char *file_name = 0);

#define INITREGTRACE()  InitRegTables()
#define REGPTR(ptr, id) RegPtr(ptr, id);
#define UNREGPTRIDEX(ptr, index)    UnregPtr(ptr, index)
#define UNREGPTR(ptr)               UnregPtr(ptr)
#define REPORTREGPTR(log_file)    ReportLivingPtr(log_file)

#else

#define INITREGTRACE()
#define REGPTR(ptr, id)
#define UNREGPTRIDEX(ptr, index)
#define UNREGPTR(ptr)
#define REPORTREGPTR(log_file)

#endif//ENABLE_PTR

#endif//H_REGTRACE_H
