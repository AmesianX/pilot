unit uCpuUsage;

interface

const
  wsMinMeasurementInterval = 250;
  { minimum amount of time that must have elapsed to calculate CPU usage, miliseconds. If time elapsed is less than this, previous result is returned, or zero, if there is no previous result. }

type
  TCPUUsageData = record
    PID, Handle: cardinal;
    oldUser, oldKernel: Int64;
    LastUpdateTime: cardinal;
    LastUsage: single;
    // Last result of wsGetCpuUsage is saved here
    Tag: cardinal;
    // Use it for anythin you like, not modified by this unit
  end;

  PCPUUsageData = ^TCPUUsageData;

function wsCreateUsageCounter(PID: cardinal): PCPUUsageData;
function wsGetCpuUsage(aCounter: PCPUUsageData): single;
procedure wsDestroyUsageCounter(aCounter: PCPUUsageData);

implementation

uses Windows;

function wsCreateUsageCounter(PID: cardinal): PCPUUsageData;
var
  p: PCPUUsageData;
  mCreationTime, mExitTime, mKernelTime, mUserTime: _FILETIME;
  h: cardinal;
begin
  result := nil;
  // We need a handle with PROCESS_QUERY_INFORMATION privileges
  h := OpenProcess(PROCESS_QUERY_INFORMATION, false, PID);
  if h = 0 then
    exit;
  new(p);
  p.PID := PID;
  p.Handle := h;
  p.LastUpdateTime := GetTickCount;
  p.LastUsage := 0;
  if GetProcessTimes(p.Handle, mCreationTime, mExitTime, mKernelTime, mUserTime)
  then
  begin
    // convert _FILETIME to Int64
    p.oldKernel := Int64(mKernelTime.dwLowDateTime or
      (mKernelTime.dwHighDateTime shr 32));
    p.oldUser := Int64(mUserTime.dwLowDateTime or
      (mUserTime.dwHighDateTime shr 32));
    result := p;
  end else begin
    dispose(p);
  end;
end;

procedure wsDestroyUsageCounter(aCounter: PCPUUsageData);
begin
  CloseHandle(aCounter.Handle);
  dispose(aCounter);
end;

function wsGetCpuUsage(aCounter: PCPUUsageData): single;
var
  mCreationTime, mExitTime, mKernelTime, mUserTime: _FILETIME;
  DeltaMs, ThisTime: cardinal;
  mKernel, mUser, mDelta: Int64;
begin
  result := aCounter.LastUsage;
  ThisTime := GetTickCount; // Get the time elapsed since last query

  DeltaMs := ThisTime - aCounter.LastUpdateTime;
  if DeltaMs < wsMinMeasurementInterval then
    exit;
  aCounter.LastUpdateTime := ThisTime;
  GetProcessTimes(aCounter.Handle, mCreationTime, mExitTime, mKernelTime,
    mUserTime);
  // convert _FILETIME to Int64.
  mKernel := Int64(mKernelTime.dwLowDateTime or
    (mKernelTime.dwHighDateTime shr 32));
  mUser := Int64(mUserTime.dwLowDateTime or (mUserTime.dwHighDateTime shr 32));
  // get the delta
  mDelta := mUser + mKernel - aCounter.oldUser - aCounter.oldKernel;

  aCounter.oldUser := mUser;
  aCounter.oldKernel := mKernel;

  result := (mDelta / DeltaMs) / 100;
  // mDelta is in units of 100 nanoseconds, so¡¦

  aCounter.LastUsage := result;
  // just in case you want to use it later, too
end;

end.
