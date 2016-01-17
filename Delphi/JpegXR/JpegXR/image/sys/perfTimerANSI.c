//*@@@+++@@@@******************************************************************
//
// Microsoft Windows Media
// Copyright (C) Microsoft Corporation. All rights reserved.
//
//*@@@---@@@@******************************************************************

//***************************************************************************
// Includes
//***************************************************************************
#include <time.h>
#include "strcodec.h"
#include "perfTimer.h"


#ifndef DISABLE_PERF_MEASUREMENT

//***************************************************************************
// Data Declarations
//***************************************************************************
typedef enum
{
    CS_UNINIT,
    CS_RUNNING,
    CS_STOPPED,
} CLOCKSTATE;

typedef struct PERFTIMERSTATE
{
    CLOCKSTATE          eState;
    PERFTIMERTIME       iElapsedTime;
    clock_t             iPrevStartTime;
    PERFTIMERTIME       iZeroTimeIntervals;
} PERFTIMERSTATE;



//***************************************************************************
// Private Functions
//***************************************************************************

Bool AccumulateTime(PERFTIMERSTATE *pState, PERFTIMERTIME *ptAccumulator)
{
    Bool        fResult = FALSE;
    clock_t     iStopTime;
    clock_t     iIntervalTime;
    iStopTime = clock();

    // Check clock result
    if ((clock_t)-1 == iStopTime)
    {
        TraceResult(WM_E_CLOCKFAILURE);
        goto exit;
    }

    iIntervalTime = (iStopTime - pState->iPrevStartTime);

    // Check for zero-time interval
    if (0 == iIntervalTime)
        pState->iZeroTimeIntervals += 1;

    // Accumulate current interval's time
    *ptAccumulator += iIntervalTime;
    fResult = TRUE;

exit:
    return fResult;
}


//***************************************************************************
// Public Functions
//***************************************************************************


Bool PerfTimerNew(PERFTIMERSTATE **ppNewPerfTimer)
{
    Bool            fResult = FALSE;
    PERFTIMERSTATE *pState = NULL;
    clock_t         ctResult;

    // Check if this clock works
    ctResult = clock();
    if ((clock_t)-1 == ctResult)
    {
        TraceResult(WM_E_CLOCKFAILURE);
        goto exit;
    }

    pState = malloc(sizeof(*pState));
    if (NULL == pState)
    {
        TraceResult(E_OUTOFMEMORY);
        goto exit;
    }
    memset(pState, 0, sizeof(*pState));
    pState->eState = CS_STOPPED;
    pState->iElapsedTime = 0;
    pState->iPrevStartTime = 0;
    pState->iZeroTimeIntervals = 0;

    *ppNewPerfTimer = pState;
    fResult = TRUE;

exit:
    assert(fResult || NULL == pState); // If error, we need to free pState
    return fResult;
} // PerfTimerNew



void PerfTimerDelete(PERFTIMERSTATE *pState)
{
    free(pState);
} // PerfTimerDelete



Bool PerfTimerStart(PERFTIMERSTATE *pState)
{
    Bool fResult = FALSE;

    if (NULL == pState)
    {
        // Can happen because we typically ignore errors and use a single bool to
        // control all perf timing (some of which can fail to init)
        goto exit;
    }

    // Make sure we are in the right state
    if (CS_STOPPED != pState->eState)
    {
        assert(FALSE);
        goto exit;
    }

    pState->iPrevStartTime = clock();

    // Check clock result
    if ((clock_t)-1 == pState->iPrevStartTime)
    {
        TraceResult(WM_E_CLOCKFAILURE);
        goto exit;
    }

    pState->eState = CS_RUNNING;
    fResult = TRUE;

exit:
    return fResult;
} // PerfTimerStart



Bool PerfTimerStop(PERFTIMERSTATE *pState)
{
    Bool        fResult = FALSE;

    if (NULL == pState)
    {
        // Can happen because we typically ignore errors and use a single bool to
        // control all perf timing (some of which can fail to init)
        goto exit;
    }

    // Make sure we are in the right state
    if (CS_RUNNING != pState->eState)
    {
        assert(FALSE);
        goto exit;
    }

    fResult = AccumulateTime(pState, &pState->iElapsedTime);
    pState->eState = CS_STOPPED;
    fResult = TRUE;

exit:
    return fResult;
} // PerfTimerStop



Bool PerfTimerGetResults(PERFTIMERSTATE *pState, PERFTIMERRESULTS *pResults)
{
    Bool            fResult = FALSE;
    PERFTIMERTIME   iElapsedTime;

    if (NULL == pState)
    {
        // Can happen because we typically ignore errors and use a single bool to
        // control all perf timing (some of which can fail to init)
        goto exit;
    }

    // Make sure we are in the right state
    if (CS_STOPPED != pState->eState && CS_RUNNING != pState->eState)
    {
        assert(FALSE);
        goto exit;
    }

    iElapsedTime = pState->iElapsedTime;
    if (CS_RUNNING == pState->eState)
    {
        // Must take a "checkpoint" time reading
        fResult = AccumulateTime(pState, &iElapsedTime);
        if (FALSE == fResult)
            goto exit;
    }

    // Convert clock ticks to nanoseconds.
    // Use floating point for ease of math. If your platform really blows
    // with floating point, replace this with appropriate integer calculation
    // based on your clock interval.
    pResults->iElapsedTime = (PERFTIMERTIME)((float)iElapsedTime *
        ((float)NANOSECONDS_PER_SECOND / (float)CLOCKS_PER_SEC));
    pResults->iTicksPerSecond = CLOCKS_PER_SEC;
    pResults->iZeroTimeIntervals = pState->iZeroTimeIntervals;
    fResult = TRUE;

exit:
    return fResult;
} // PerfTimerGetResults



Bool PerfTimerCopyStartTime(PERFTIMERSTATE *pDestPerfTimer,
                            PERFTIMERSTATE *pSrcPerfTimer)
{
    Bool    fResult = FALSE;

    if (NULL == pDestPerfTimer)
    {
        TraceResult(E_INVALIDARG);
        goto exit;
    }

    if (NULL == pSrcPerfTimer)
    {
        TraceResult(E_INVALIDARG);
        goto exit;
    }

    // Check that both timers are in proper state - both must be running
    if (CS_RUNNING != pDestPerfTimer->eState)
    {
        TraceResult(WM_E_INVALIDSTATE);
        goto exit;
    }

    if (CS_RUNNING != pSrcPerfTimer->eState)
    {
        TraceResult(WM_E_INVALIDSTATE);
        goto exit;
    }

    if (0 != pDestPerfTimer->iElapsedTime)
    {
        // If iElapsedTime is non-zero, caller won't get what he is expecting
        // when he calls PerfTimerGetResults
        TraceResult(WM_E_INVALIDSTATE);
        goto exit;
    }

    pDestPerfTimer->iPrevStartTime = pSrcPerfTimer->iPrevStartTime;
    fResult = TRUE;

exit:
    return fResult;
} // PerfTimerCopyStartTime

#endif // DISABLE_PERF_MEASUREMENT
