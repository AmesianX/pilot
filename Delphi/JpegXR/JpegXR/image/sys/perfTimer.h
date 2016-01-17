//*@@@+++@@@@******************************************************************
//
// Microsoft Windows Media
// Copyright (C) Microsoft Corporation. All rights reserved.
//
//*@@@---@@@@******************************************************************

#ifndef __PERFTIMER_H_
#define __PERFTIMER_H_

//***************************************************************************
// Description
//
//   Performance timer API used to measure codec performance. The underlying
// implementation of this API may vary - from ANSI-C implementation via clock,
// Win32 implementation via QueryPerformanceCounter or GetProcessTimes. At
// present we only support one implementation of this PerfTimer "object".
// You choose the implementation by choosing which one of the many files
// to compile and link with your application.
//***************************************************************************

#ifdef DISABLE_PERF_MEASUREMENT

#define PERFTIMER_ONLY(code)
#define PERFTIMER_NEW(fPerf, ppPerfTimer)
#define PERFTIMER_DELETE(fPerf, ppPerfTimer)
#define PERFTIMER_START(fPerf, pPerfTimer)
#define PERFTIMER_STOP(fPerf, pPerfTimer)
#define PERFTIMER_GETRESULTS(fPerf, pPerfTimer, pResults)
#define PERFTIMER_COPYSTARTTIME(fPerf, pDst, pSrc)
#define PERFTIMER_REPORT(fPerf, pCodec)

#else // DISABLE_PERF_MEASUREMENT

#define PERFTIMER_ONLY(code) code
#define PERFTIMER_NEW(fPerf, ppPerfTimer)    if (fPerf) {Bool b = PerfTimerNew(ppPerfTimer); assert(b);};
#define PERFTIMER_DELETE(fPerf, pPerfTimer)  if (fPerf) {PerfTimerDelete(pPerfTimer);};
#define PERFTIMER_START(fPerf, pPerfTimer)   if (fPerf) {Bool b = PerfTimerStart(pPerfTimer); assert(b);};
#define PERFTIMER_STOP(fPerf, pPerfTimer)    if (fPerf) {Bool b = PerfTimerStop(pPerfTimer); assert(b);};
#define PERFTIMER_GETRESULTS(fPerf, pPerfTimer, pResults) \
    if (fPerf) {Bool b = PerfTimerGetResults((pPerfTimer), (pResults)); assert(b);};
#define PERFTIMER_COPYSTARTTIME(fPerf, pDst, pSrc) \
    if (fPerf) {Bool b = PerfTimerCopyStartTime((pDst), (pSrc)); assert(b);};
#define PERFTIMER_REPORT(fPerf, pCodec) \
    if (fPerf) {OutputPerfTimerReport(pCodec);};
#endif // DISABLE_PERF_MEASUREMENT

//***************************************************************************
// Data Types
//***************************************************************************
typedef U64 PERFTIMERTIME;
typedef struct PERFTIMERRESULTS
{
    PERFTIMERTIME   iElapsedTime;       // In nanoseconds or CPU cycles
    PERFTIMERTIME   iTicksPerSecond;    // Number of ticks per second (clock frequency)
    PERFTIMERTIME   iZeroTimeIntervals; // Number of zero-time intervals.
        // Presence of zero-time intervals may indicate insufficient clock precision
} PERFTIMERRESULTS;

#define NANOSECONDS_PER_SECOND  1000000000


//***************************************************************************
// Functions and Macros
//***************************************************************************
Bool PerfTimerNew(struct PERFTIMERSTATE **ppNewPerfTimer);
void PerfTimerDelete(struct PERFTIMERSTATE *pThisPerfTimer);
Bool PerfTimerStart(struct PERFTIMERSTATE *pThisPerfTimer);
Bool PerfTimerStop(struct PERFTIMERSTATE *pThisPerfTimer);
Bool PerfTimerGetResults(struct PERFTIMERSTATE *pThisPerfTimer,
                         PERFTIMERRESULTS *pPerfTimerResults);
Bool PerfTimerCopyStartTime(struct PERFTIMERSTATE *pDestPerfTimer,
                            struct PERFTIMERSTATE *pSrcPerfTimer);

#endif // __PERFTIMER_H_
