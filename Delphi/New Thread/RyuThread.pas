unit RyuThread;

interface

uses
  Winapi.Windows, System.RTLConsts, Classes, SysUtils;

type
  TRyuThread = class
  private type
    PSynchronizeRecord = ^TSynchronizeRecord;
    TSynchronizeRecord = record
      FThread: TObject;
      FMethod: TThreadMethod;
      FProcedure: TThreadProcedure;
      FSynchronizeException: TObject;
    end;
  private class var
    FProcessorCount: Integer;
  private
{$IFDEF MSWINDOWS}
    FHandle: THandle platform;
{$ENDIF}
    FThreadID: TThreadID;
{$IFDEF POSIX}
{$IFDEF LINUX}
    FCreateSuspendedSem: TSemaphore;
{$ENDIF}
{$IFDEF MACOS}
    FCreateSuspendedEvent: MPEventID;
{$ENDIF}
    FInitialSuspendDone: Boolean;
{$ENDIF}
    FCreateSuspended: Boolean;
    FTerminated: Boolean;
    FSuspended: Boolean;
    FFreeOnTerminate: Boolean;
    FFinished: Boolean;
    FReturnValue: Integer;
    FOnTerminate: TNotifyEvent;
    FSynchronize: TSynchronizeRecord;
    FFatalException: TObject;
    FExternalThread: Boolean;
    class constructor Create;
    class destructor Destroy;
    procedure CallOnTerminate;
    class procedure Synchronize(ASyncRec: PSynchronizeRecord; QueueEvent: Boolean = False); overload;
    class function GetCurrentThread: TRyuThread; static;
    class function GetIsSingleProcessor: Boolean; static; inline;
    procedure InternalStart(Force: Boolean);
{$IFDEF MSWINDOWS}
    function GetPriority: TThreadPriority; platform;
    procedure SetPriority(Value: TThreadPriority); platform;
{$ENDIF}
{$IFDEF POSIX}
    // ** Priority is an Integer value in Linux
    function GetPriority: Integer; platform;
    procedure SetPriority(Value: Integer); platform;
    function GetPolicy: Integer; platform;
    procedure SetPolicy(Value: Integer); platform;
{$ENDIF}
    procedure SetSuspended(Value: Boolean);
  private class threadvar
    FCurrentThread: TRyuThread;
  protected
    procedure CheckThreadError(ErrCode: Integer); overload;
    procedure CheckThreadError(Success: Boolean); overload;
    procedure DoTerminate; virtual;
    procedure TerminatedSet; virtual;
    procedure Execute; virtual; abstract;
    procedure Queue(AMethod: TThreadMethod); overload;
    procedure Synchronize(AMethod: TThreadMethod); overload;
    procedure Queue(AThreadProc: TThreadProcedure); overload;
    procedure Synchronize(AThreadProc: TThreadProcedure); overload;
    property ReturnValue: Integer read FReturnValue write FReturnValue;
    property Terminated: Boolean read FTerminated;
  public
    class var StackSize : integer;

    constructor Create; overload;
    constructor Create(CreateSuspended: Boolean); overload;
    destructor Destroy; override;
    // CreateAnonymousThread will create an instance of an internally derived TRyuThread that simply will call the
    // anonymous method of type TProc. This thread is created as suspended, so you should call the Start method
    // to make the thread run. The thread is also marked as FreeOnTerminate, so you should not touch the returned
    // instance after calling Start as it could have run and is then freed before another external calls or
    // operations on the instance are attempted.
    class function CreateAnonymousThread(const ThreadProc: TProc): TRyuThread; static;
    procedure AfterConstruction; override;
    // This function is not intended to be used for thread synchronization.
    procedure Resume; deprecated;
    // Use Start after creating a suspended thread.
    procedure Start;
    // This function is not intended to be used for thread synchronization.
    procedure Suspend; deprecated;
    procedure Terminate;
    function WaitFor: LongWord;
    // NOTE: You can only call CheckTerminated and SetReturnValue on an internally created thread.
    // Calling this from an externally created thread will raise an exception
    // Use TRyuThread.CheckTerminated to check if the Terminated flag has been set on the current thread
    class function CheckTerminated: Boolean; static;
    // Use TRyuThread.SetReturnValue to set the current thread's return value from code that doesn't have
    // direct access to the current thread
    class procedure SetReturnValue(Value: Integer); static;
    class procedure Queue(AThread: TRyuThread; AMethod: TThreadMethod); overload; static;
    class procedure RemoveQueuedEvents(AThread: TRyuThread; AMethod: TThreadMethod); overload; static;
    class procedure StaticQueue(AThread: TRyuThread; AMethod: TThreadMethod); static; deprecated 'From C++ just use Queue now that it is just a static method';
    class procedure Synchronize(AThread: TRyuThread; AMethod: TThreadMethod); overload; static;
    class procedure StaticSynchronize(AThread: TRyuThread; AMethod: TThreadMethod); static; deprecated 'From C++ just use Synchronize now that it is just a static method';
    class procedure Queue(AThread: TRyuThread; AThreadProc: TThreadProcedure); overload; static;
    class procedure Synchronize(AThread: TRyuThread; AThreadProc: TThreadProcedure); overload; static;
    class procedure RemoveQueuedEvents(AThread: TRyuThread); overload; static;
    class procedure RemoveQueuedEvents(AMethod: TThreadMethod); overload; static;
    class procedure NameThreadForDebugging(AThreadName: AnsiString; AThreadID: TThreadID = TThreadID(-1)); static;
    class procedure SpinWait(Iterations: Integer); static;
    class procedure Sleep(Timeout: Integer); static;
    class procedure Yield; static;
    property ExternalThread: Boolean read FExternalThread;
    property FatalException: TObject read FFatalException;
    property FreeOnTerminate: Boolean read FFreeOnTerminate write FFreeOnTerminate;
    property Finished: Boolean read FFinished;
{$IFDEF MSWINDOWS}
    property Handle: THandle read FHandle;
    property Priority: TThreadPriority read GetPriority write SetPriority;
{$ENDIF}
{$IFDEF POSIX}
    // ** Priority is an Integer **
    property Priority: Integer read GetPriority write SetPriority;
    property Policy: Integer read GetPolicy write SetPolicy;
{$ENDIF}
    property Suspended: Boolean read FSuspended write SetSuspended;
    property ThreadID: TThreadID read FThreadID;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
    class property CurrentThread: TRyuThread read GetCurrentThread;
    class property ProcessorCount: Integer read FProcessorCount;
    class property IsSingleProcessor: Boolean read GetIsSingleProcessor;
  end;


implementation

type
  TSyncProc = record
    SyncRec: TRyuThread.PSynchronizeRecord;
    Queued: Boolean;
    Signal: TObject;
  end;
  PSyncProc = ^TSyncProc;

  TExternalThread = class(TRyuThread)
  protected
    procedure Execute; override; // This never runs.
  public
    constructor Create;
  end;

  TAnonymousThread = class(TRyuThread)
  private
    FProc: TProc;
  protected
    procedure Execute; override;
  public
    constructor Create(const AProc: TProc);
  end;

{ TExternalThread }

constructor TExternalThread.Create;
begin
  FExternalThread := True;
  inherited Create(False);
end;

procedure TExternalThread.Execute;
begin
  // nothing happening here.
end;

{ TAnonymousThread }

constructor TAnonymousThread.Create(const AProc: TProc);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FProc := AProc;
end;

procedure TAnonymousThread.Execute;
begin
  FProc();
end;
var
  SyncList: TList = nil;
  ThreadLock: TObject;
  ExternalThreads: TThreadList;

procedure InitThreadSynchronization;
begin
  ThreadLock := TObject.Create;
{$IF Defined(MSWINDOWS)}
  SyncEvent := CreateEvent(nil, True, False, '');
  if SyncEvent = 0 then
    RaiseLastOSError;
{$ELSEIF DEFINED(POSIX)}
  if pipe(SyncEvent) < 0 then
    RaiseLastOSError;
{$IFEND}
end;

procedure DoneThreadSynchronization;
begin
  ThreadLock.Free;
{$IF Defined(MSWINDOWS)}
  CloseHandle(SyncEvent);
{$ELSEIF Defined(POSIX)}
  __close(SyncEvent.ReadDes);
  __close(SyncEvent.WriteDes);
{$IFEND}
end;


procedure FreeExternalThreads;
var
  I: Integer;
  LExternalThreads: TThreadList;
begin
  LExternalThreads := InterlockedExchangePointer(Pointer(ExternalThreads), nil);
  if LExternalThreads <> nil then
    with LExternalThreads.LockList do
    try
      for I := 0 to Count - 1 do
        TObject(Items[I]).Free;
    finally
      LExternalThreads.UnlockList;
    end;
  LExternalThreads.Free;
end;

procedure ResetSyncEvent;
{$IF Defined(LINUX)}
var
  nRead: Integer;
  Dummy: Byte;
{$IFEND}
begin
{$IF Defined(MSWINDOWS)}
  ResetEvent(SyncEvent);
{$ELSEIF Defined(LINUX)}
  if (ioctl(SyncEvent.ReadDes, FIONREAD, @nRead) = 0) and (nRead > 0) then
    __read(SyncEvent.ReadDes, @Dummy, SizeOf(Dummy));
{$IFEND}
end;

procedure WaitForSyncEvent(Timeout: Integer);
{$IF Defined(LINUX)}
var
  EventFds: TFDSet;
  Tm: TTimeVal;
{$IFEND}
begin
{$IF Defined(MSWINDOWS)}
  if WaitForSingleObject(SyncEvent, Timeout) = WAIT_OBJECT_0 then
    ResetSyncEvent;
{$ELSEIF Defined(LINUX)}
  FD_ZERO(EventFds);
  FD_SET(SyncEvent.ReadDes, EventFds);
  Tm.tv_sec := Timeout div 1000;
  Tm.tv_usec := (Timeout mod 1000) * 1000;
  if select(SyncEvent.ReadDes + 1, @EventFds, nil, nil, @Tm) > 0 then
    ResetSyncEvent;
{$IFEND}
end;

procedure SignalSyncEvent;
{$IF Defined(LINUX)}
const
  Dummy: Byte = 0;
var
  nRead: Integer;
{$IFEND}
begin
{$IF Defined(MSWINDOWS)}
  SetEvent(SyncEvent);
{$ELSEIF Defined(LINUX)}
  if (ioctl(SyncEvent.ReadDes, FIONREAD, @nRead) = 0) and (nRead = 0) then
    __write(SyncEvent.WriteDes, Dummy, SizeOf(Dummy));
{$IFEND}
end;

function CheckSynchronize(Timeout: Integer = 0): Boolean;
var
  SyncProc: PSyncProc;
  LocalSyncList: TList;
begin
{$IFDEF MSWINDOWS}
  if SyncEvent = 0 then
    Exit(False);
{$ENDIF}
{$IFDEF POSIX}
  if (SyncEvent.ReadDes = 0) or (SyncEvent.WriteDes = 0) then
    Exit(False);
{$ENDIF}
  if TRyuThread.CurrentThread.ThreadID <> MainThreadID then
    raise EThread.CreateResFmt(@SCheckSynchronizeError, [TRyuThread.CurrentThread.ThreadID]);
  if Timeout > 0 then
    WaitForSyncEvent(Timeout)
  else
    ResetSyncEvent;
  LocalSyncList := nil;
  TMonitor.Enter(ThreadLock);
  try
    Pointer(LocalSyncList) := InterlockedExchangePointer(Pointer(SyncList), Pointer(LocalSyncList));
    try
      Result := (LocalSyncList <> nil) and (LocalSyncList.Count > 0);
      if Result then
      begin
        while LocalSyncList.Count > 0 do
        begin
          SyncProc := LocalSyncList[0];
          LocalSyncList.Delete(0);
          TMonitor.Exit(ThreadLock);
          try
            try
              if Assigned(SyncProc.SyncRec.FMethod) then
                SyncProc.SyncRec.FMethod()
              else if Assigned(SyncProc.SyncRec.FProcedure) then
                SyncProc.SyncRec.FProcedure();
            except
              if not SyncProc.Queued then
                SyncProc.SyncRec.FSynchronizeException := AcquireExceptionObject
              else
                raise;
            end;
          finally
            TMonitor.Enter(ThreadLock);
          end;
          if not SyncProc.Queued then
            TMonitor.Pulse(SyncProc.Signal)
          else
          begin
            Dispose(SyncProc.SyncRec);
            Dispose(SyncProc);
          end;
        end;
      end;
    finally
      LocalSyncList.Free;
    end;
  finally
    TMonitor.Exit(ThreadLock);
  end;
end;

function ThreadProc(Thread: TRyuThread): Integer;
var
  FreeThread: Boolean;
{$IFDEF MACOS}
  Flags: MPEventFlags;
{$ENDIF}
begin
  TRyuThread.FCurrentThread := Thread;
{$IFDEF LINUX}
  if Thread.FSuspended then sem_wait(Thread.FCreateSuspendedSem);
{$ENDIF}
{$IFDEF MACOS}
  if Thread.FSuspended then MPWaitForEvent(Thread.FCreateSuspendedEvent, Flags, kDurationForever);
{$ENDIF}
  try
    if not Thread.Terminated then
    try
      Thread.Execute;
    except
      Thread.FFatalException := AcquireExceptionObject;
    end;
  finally
    Result := Thread.FReturnValue;
    FreeThread := Thread.FFreeOnTerminate;
    Thread.DoTerminate;
    Thread.FFinished := True;
    SignalSyncEvent;
    if FreeThread then Thread.Free;
{$IFDEF MSWINDOWS}
    EndThread(Result);
{$ENDIF}
{$IFDEF POSIX}
    // Directly call pthread_exit since EndThread will detach the thread causing
    // the pthread_join in TRyuThread.WaitFor to fail.  Also, make sure the EndThreadProc
    // is called just like EndThread would do. EndThreadProc should not return
    // and call pthread_exit itself.
    if Assigned(EndThreadProc) then
      EndThreadProc(Result);
    pthread_exit(Result);
{$ENDIF}
  end;
end;

constructor TRyuThread.Create;
begin
  Create(False);
end;

constructor TRyuThread.Create(CreateSuspended: Boolean);
{$IFDEF POSIX}
var
  ErrCode: Integer;
{$ENDIF}
begin
  inherited Create;
  FSuspended := not FExternalThread;
  FCreateSuspended := CreateSuspended and not FExternalThread;
  if not FExternalThread then
  begin
{$IFDEF MSWINDOWS}
    FHandle := BeginThread(nil, StackSize, @ThreadProc, Pointer(Self), CREATE_SUSPENDED, FThreadID);
    if FHandle = 0 then
      raise EThread.CreateResFmt(@SThreadCreateError, [SysErrorMessage(GetLastError)]);
{$ENDIF}
{$IFDEF POSIX}
{$IFDEF LINUX}
    sem_init(FCreateSuspendedSem, False, 0);
{$ELSE}
    MPCreateEvent(FCreateSuspendedEvent);
{$ENDIF}
    ErrCode := BeginThread(nil, @ThreadProc, Pointer(Self), FThreadID);
    if ErrCode <> 0 then
      raise EThread.CreateResFmt(@SThreadCreateError, [SysErrorMessage(ErrCode)]);
{$ENDIF}
  end else
  begin
{$IFDEF MSWINDOWS}
    FHandle := Winapi.Windows.GetCurrentThread;
{$ENDIF}
    FThreadId := GetCurrentThreadId;
  end;
end;

class constructor TRyuThread.Create;
begin
  InitThreadSynchronization;
  FProcessorCount := System.CPUCount;
end;

class function TRyuThread.CreateAnonymousThread(const ThreadProc: TProc): TRyuThread;
begin
  Result := TAnonymousThread.Create(ThreadProc);
end;

destructor TRyuThread.Destroy;
begin
  if (FThreadID <> 0) and not FFinished and not FExternalThread then
  begin
    Terminate;
    if FCreateSuspended or FSuspended then
      Resume;
    WaitFor;
  end;
  RemoveQueuedEvents(Self);
{$IFDEF MSWINDOWS}
  if (FHandle <> 0) and not FExternalThread then CloseHandle(FHandle);
{$ENDIF}
{$IFDEF POSIX}
  // This final check is to ensure that even if the thread was never waited on
  // its resources will be freed.
  if (FThreadID <> 0) and not FExternalThread then pthread_detach(pthread_t(FThreadID));
{$IFDEF LINUX}
  sem_destroy(FCreateSuspendedSem);
{$ELSE}
  MPDeleteEvent(FCreateSuspendedEvent);
{$ENDIF}
{$ENDIF}
  inherited Destroy;
  FFatalException.Free;
end;

class destructor TRyuThread.Destroy;
begin
  FreeAndNil(SyncList);
  FreeExternalThreads;
  DoneThreadSynchronization;
end;

procedure TRyuThread.AfterConstruction;
begin
  if not FCreateSuspended and not FExternalThread then
    InternalStart(True);
end;

class function TRyuThread.CheckTerminated: Boolean;
var
  Thread: TRyuThread;
begin
  Thread := CurrentThread;
  if Thread is TExternalThread then
    raise EThreadExternalException.CreateRes(@SThreadExternalCheckTerminated);
  Result := Thread.Terminated;
end;

procedure TRyuThread.CheckThreadError(ErrCode: Integer);
begin
  if ErrCode <> 0 then
    raise EThread.CreateResFmt(@SThreadError, [SysErrorMessage(ErrCode), ErrCode]);
end;

procedure TRyuThread.CheckThreadError(Success: Boolean);
begin
  if not Success then
    CheckThreadError(GetLastError);
end;

procedure TRyuThread.CallOnTerminate;
begin
  if Assigned(FOnTerminate) then FOnTerminate(Self);
end;

procedure TRyuThread.DoTerminate;
begin
  if Assigned(FOnTerminate) then Synchronize(CallOnTerminate);
end;

procedure TRyuThread.TerminatedSet;
begin
end;

class function TRyuThread.GetCurrentThread: TRyuThread;
var
  ExternalThread: TRyuThread;
  LExternalThreads: TThreadList;
begin
  if FCurrentThread = nil then
  begin
    ExternalThread := TExternalThread.Create;
    if ExternalThreads = nil then
    begin
      LExternalThreads := TThreadList.Create;
      if InterlockedCompareExchangePointer(Pointer(ExternalThreads), LExternalThreads, nil) <> nil then
        LExternalThreads.Free;
    end;
    ExternalThreads.Add(ExternalThread);
    FCurrentThread := ExternalThread;
  end;
  Result := FCurrentThread;
end;

class function TRyuThread.GetIsSingleProcessor: Boolean;
begin
  Result := FProcessorCount < 2;
end;

procedure TRyuThread.InternalStart(Force: Boolean);
begin
  if (FCreateSuspended or Force) and not FFinished and not FExternalThread then
  begin
    FSuspended := False;
    FCreateSuspended := False;
{$IFDEF MSWINDOWS}
    if ResumeThread(FHandle) <> 1 then
      raise EThread.Create(SThreadStartError);
{$ENDIF}
{$IFDEF LINUX}
    sem_post(FCreateSuspendSem);
{$ENDIF}
{$IFDEF MACOS}
    MPSetEvent(FCreateSuspendedEvent, 1);
{$ENDIF}
  end else
    raise EThread.Create(SThreadStartError);
end;

{$IFDEF MSWINDOWS}
const
  Priorities: array [TThreadPriority] of Integer =
   (THREAD_PRIORITY_IDLE, THREAD_PRIORITY_LOWEST, THREAD_PRIORITY_BELOW_NORMAL,
    THREAD_PRIORITY_NORMAL, THREAD_PRIORITY_ABOVE_NORMAL,
    THREAD_PRIORITY_HIGHEST, THREAD_PRIORITY_TIME_CRITICAL);

function TRyuThread.GetPriority: TThreadPriority;
var
  P: Integer;
  I: TThreadPriority;
begin
  P := GetThreadPriority(FHandle);
  CheckThreadError(P <> THREAD_PRIORITY_ERROR_RETURN);
  Result := tpNormal;
  for I := Low(TThreadPriority) to High(TThreadPriority) do
    if Priorities[I] = P then Result := I;
end;

procedure TRyuThread.SetPriority(Value: TThreadPriority);
begin
  CheckThreadError(SetThreadPriority(FHandle, Priorities[Value]));
end;
{$ENDIF}
{$IFDEF POSIX}
function TRyuThread.GetPriority: Integer;
var
  P: Integer;
  J: sched_param;
begin
  {
    Posix Priority is based on the Schedule policy.
    There are 3 different kinds of policy.  See SetPolicy.

        Policy          Type         Priority
      ----------      --------       --------
      SCHED_RR        RealTime         1-99
      SCHED_FIFO      RealTime         1-99
      SCHED_OTHER     Regular           0

    SCHED_RR and SCHED_FIFO can only be set by root.
  }
  CheckThreadError(pthread_getschedparam(pthread_t(FThreadID), P, J));
  Result := J.sched_priority;
end;

{
  Note that to fully utilize Posix Scheduling, see SetPolicy.
}
procedure TRyuThread.SetPriority(Value: Integer);
var
  P: sched_param;
begin
  if Value <> Priority then
  begin
    P.sched_priority := Value;
    CheckThreadError(pthread_setschedparam(pthread_t(FThreadID), Policy, P));
  end;
end;

function TRyuThread.GetPolicy: Integer;
var
  J: sched_param;
begin
  CheckThreadError(pthread_getschedparam(pthread_t(FThreadID), Result, J));
end;

{
  Note that to fully utilize Posix Scheduling, SetPolicy needs to
  be used as well.  See SetPriority for the relationship between these
  methods.
}
procedure TRyuThread.SetPolicy(Value: Integer);
var
  P: sched_param;
begin
  if Value <> Policy then
  begin
    P.sched_priority := GetPriority;
    CheckThreadError(pthread_setschedparam(pthread_t(FThreadID), Value, P));
  end;
end;
{$ENDIF}

procedure TRyuThread.Queue(AMethod: TThreadMethod);
var
  LSynchronize: PSynchronizeRecord;
begin
  New(LSynchronize);
  try
    LSynchronize.FThread := Self;
    LSynchronize.FSynchronizeException := nil;
    LSynchronize.FMethod := AMethod;
    Synchronize(LSynchronize, True);
  finally
    if MainThreadID = CurrentThread.ThreadID then
      Dispose(LSynchronize);
  end;
end;

procedure TRyuThread.Queue(AThreadProc: TThreadProcedure);
var
  LSynchronize: PSynchronizeRecord;
begin
  New(LSynchronize);
  try
    LSynchronize.FThread := Self;
    LSynchronize.FSynchronizeException := nil;
    LSynchronize.FMethod := nil;
    LSynchronize.FProcedure := AThreadProc;
    Synchronize(LSynchronize, True);
  finally
    if MainThreadID = CurrentThread.ThreadID then
      Dispose(LSynchronize);
  end;
end;

class procedure TRyuThread.Queue(AThread: TRyuThread; AMethod: TThreadMethod);
var
  LSynchronize: PSynchronizeRecord;
begin
  if AThread <> nil then
    AThread.Queue(AMethod)
  else
  begin
    New(LSynchronize);
    try
      LSynchronize.FThread := nil;
      LSynchronize.FSynchronizeException := nil;
      LSynchronize.FMethod := AMethod;
      Synchronize(LSynchronize, True);
    finally
      if MainThreadID = CurrentThread.ThreadID then
        Dispose(LSynchronize);
    end;
  end;
end;

class procedure TRyuThread.RemoveQueuedEvents(AThread: TRyuThread; AMethod: TThreadMethod);
var
  I: Integer;
  SyncProc: PSyncProc;
begin
  TMonitor.Enter(ThreadLock);
  try
    if SyncList <> nil then
      for I := SyncList.Count - 1 downto 0 do
      begin
        SyncProc := SyncList[I];
        if (SyncProc.Signal = nil) and
          (((AThread <> nil) and (SyncProc.SyncRec.FThread = AThread)) or
            (Assigned(AMethod) and (TMethod(SyncProc.SyncRec.FMethod).Code = TMethod(AMethod).Code) and
             (TMethod(SyncProc.SyncRec.FMethod).Data = TMethod(AMethod).Data))) then
        begin
          SyncList.Delete(I);
          Dispose(SyncProc.SyncRec);
          Dispose(SyncProc);
        end;
      end;
  finally
    TMonitor.Exit(ThreadLock);
  end;
end;

class procedure TRyuThread.SetReturnValue(Value: Integer);
var
  Thread: TRyuThread;
begin
  Thread := CurrentThread;
  if Thread is TExternalThread then
    raise EThreadExternalException.CreateRes(@SThreadExternalSetReturnValue);
  Thread.ReturnValue := Value;
end;

class procedure TRyuThread.StaticQueue(AThread: TRyuThread; AMethod: TThreadMethod);
begin
  Queue(AThread, AMethod);
end;

class procedure TRyuThread.Synchronize(ASyncRec: PSynchronizeRecord; QueueEvent: Boolean = False);
var
  SyncProc: TSyncProc;
  SyncProcPtr: PSyncProc;
begin
  if CurrentThread.ThreadID = MainThreadID then
  begin
    if Assigned(ASyncRec.FMethod) then
      ASyncRec.FMethod()
    else if Assigned(ASyncRec.FProcedure) then
      ASyncRec.FProcedure();
  end else
  begin
    if QueueEvent then
      New(SyncProcPtr)
    else
      SyncProcPtr := @SyncProc;
    if not QueueEvent then
      SyncProcPtr.Signal := TObject.Create
    else
      SyncProcPtr.Signal := nil;
    try
      TMonitor.Enter(ThreadLock);
      try
        SyncProcPtr.Queued := QueueEvent;
        if SyncList = nil then
          SyncList := TList.Create;
        SyncProcPtr.SyncRec := ASyncRec;
        SyncList.Add(SyncProcPtr);
        SignalSyncEvent;
        if Assigned(WakeMainThread) then
          WakeMainThread(SyncProcPtr.SyncRec.FThread);
        if not QueueEvent then
          TMonitor.Wait(SyncProcPtr.Signal, ThreadLock, INFINITE)
      finally
        TMonitor.Exit(ThreadLock);
      end;
    finally
      if not QueueEvent then
        SyncProcPtr.Signal.Free;
    end;
    if not QueueEvent and Assigned(ASyncRec.FSynchronizeException) then
      raise ASyncRec.FSynchronizeException;
  end;
end;

procedure TRyuThread.Synchronize(AMethod: TThreadMethod);
begin
  FSynchronize.FThread := Self;
  FSynchronize.FSynchronizeException := nil;
  FSynchronize.FMethod := AMethod;
  FSynchronize.FProcedure := nil;
  Synchronize(@FSynchronize);
end;

procedure TRyuThread.Synchronize(AThreadProc: TThreadProcedure);
begin
  FSynchronize.FThread := Self;
  FSynchronize.FSynchronizeException := nil;
  FSynchronize.FMethod := nil;
  FSynchronize.FProcedure := AThreadProc;
  Synchronize(@FSynchronize);
end;

class procedure TRyuThread.Synchronize(AThread: TRyuThread; AMethod: TThreadMethod);
var
  SyncRec: TSynchronizeRecord;
begin
  if AThread <> nil then
    AThread.Synchronize(AMethod)
  else
  begin
    SyncRec.FThread := nil;
    SyncRec.FSynchronizeException := nil;
    SyncRec.FMethod := AMethod;
    SyncRec.FProcedure := nil;
    TRyuThread.Synchronize(@SyncRec);
  end;
end;

class procedure TRyuThread.Queue(AThread: TRyuThread; AThreadProc: TThreadProcedure);
var
  LSynchronize: PSynchronizeRecord;
begin
  if AThread <> nil then
    AThread.Queue(AThreadProc)
  else
  begin
    New(LSynchronize);
    try
      LSynchronize.FThread := nil;
      LSynchronize.FSynchronizeException := nil;
      LSynchronize.FMethod := nil;
      LSynchronize.FProcedure := AThreadProc;
      Synchronize(LSynchronize, True);
    finally
      if MainThreadID = CurrentThread.ThreadID then
        Dispose(LSynchronize);
    end;
  end;
end;

class procedure TRyuThread.Synchronize(AThread: TRyuThread; AThreadProc: TThreadProcedure);
var
  SyncRec: TSynchronizeRecord;
begin
  if AThread <> nil then
    AThread.Synchronize(AThreadProc)
  else
  begin
    SyncRec.FThread := nil;
    SyncRec.FSynchronizeException := nil;
    SyncRec.FMethod := nil;
    SyncRec.FProcedure := AThreadProc;
    TRyuThread.Synchronize(@SyncRec);
  end;
end;

class procedure TRyuThread.StaticSynchronize(AThread: TRyuThread; AMethod: TThreadMethod);
begin
  Synchronize(AThread, AMethod);
end;

class procedure TRyuThread.RemoveQueuedEvents(AThread: TRyuThread);
var
  I: Integer;
  SyncProc: PSyncProc;
begin
  TMonitor.Enter(ThreadLock);
  try
    if SyncList <> nil then
      for I := SyncList.Count - 1 downto 0 do
      begin
        SyncProc := SyncList[I];
        if (SyncProc.Signal = nil) and
          ((AThread <> nil) and (SyncProc.SyncRec.FThread = AThread)) then
        begin
          SyncList.Delete(I);
          Dispose(SyncProc.SyncRec);
          Dispose(SyncProc);
        end;
      end;
  finally
    TMonitor.Exit(ThreadLock);
  end;
end;

class procedure TRyuThread.RemoveQueuedEvents(AMethod: TThreadMethod);
begin
  RemoveQueuedEvents(nil, AMethod);
end;

procedure TRyuThread.SetSuspended(Value: Boolean);
begin
  if Value <> FSuspended then
    if Value then
      Suspend
    else
      Resume;
end;

class procedure TRyuThread.SpinWait(Iterations: Integer);
{$IF defined(X86ASM) or defined(X64ASM)}
asm
    CMP  Iterations, 0
    JNG  @Done
@Loop:
    PAUSE
    DEC  Iterations
    CMP  Iterations, 0
    JG   @Loop
@Done:
end;
{$ELSE}
begin
  while Iterations > 0 do
  begin
    System.YieldProcessor;
    Dec(Iterations);
  end;
end;
{$IFEND}

class procedure TRyuThread.Sleep(Timeout: Integer);
begin
{$IFDEF POSIX}
  usleep(Timeout * 1000);
{$ENDIF}
{$IFDEF MSWINDOWS}
  Winapi.Windows.Sleep(Timeout);
{$ENDIF}
end;

class procedure TRyuThread.Yield;
begin
{$IFDEF POSIX}
  sched_yield;
{$ENDIF}
{$IFDEF MSWINDOWS}
  SwitchToThread;
{$ENDIF}
end;

procedure TRyuThread.Start;
begin
  InternalStart(False);
end;

procedure TRyuThread.Suspend;
var
  OldSuspend: Boolean;
begin
  OldSuspend := FSuspended;
  try
    FSuspended := True;
{$IFDEF MSWINDOWS}
    CheckThreadError(Integer(SuspendThread(FHandle)) >= 0);
{$ENDIF}
{$IFDEF LINUX}
    CheckThreadError(pthread_kill(pthread_t(FThreadID), SIGSTOP));
{$ENDIF}
{$IFDEF MACOS}
    CheckThreadError(thread_suspend(pthread_mach_thread_np(pthread_t(FThreadID))));
{$ENDIF MACOS}
  except
    FSuspended := OldSuspend;
    raise;
  end;
end;

{$IFDEF MSWINDOWS}
procedure TRyuThread.Resume;
var
  SuspendCount: Integer;
begin
  SuspendCount := ResumeThread(FHandle);
  CheckThreadError(SuspendCount >= 0);
  if SuspendCount = 1 then
    FSuspended := False;
end;
{$ENDIF}

{$IFDEF LINUX}
{
  About Suspend and Resume. POSIX does not support suspending/resuming a thread.
  Suspending a thread is considerd dangerous since it is not guaranteed where the
  thread would be suspend. It might be holding a lock, mutex or it might be inside
  a critical section.  In order to simulate it in Linux we've used signals. To
  suspend, a thread SIGSTOP is sent and to resume, SIGCONT is sent. Note that this
  is Linux only i.e. according to POSIX if a thread receives SIGSTOP then the
  entire process is stopped. However Linux doesn't entirely exhibit the POSIX-mandated
  behaviour. If and when it fully complies with the POSIX standard then suspend
  and resume won't work.
}
procedure TRyuThread.Resume;
begin
  if not FInitialSuspendDone then
  begin
    FInitialSuspendDone := True;
    sem_post(FCreateSuspendedSem);
  end else
    CheckThreadError(pthread_kill(pthread_t(FThreadID), SIGCONT));
  FSuspended := False;
end;
{$ENDIF}

{$IFDEF MACOS}
procedure TRyuThread.Resume;
begin
  if not FInitialSuspendDone then
  begin
    FInitialSuspendDone := True;
    MPSetEvent(FCreateSuspendedEvent, 1);
  end else
    CheckThreadError(thread_resume(pthread_mach_thread_np(pthread_t(FThreadID))));
  FSuspended := False;
end;
{$ENDIF}

procedure TRyuThread.Terminate;
begin
  if FExternalThread then
    raise EThread.CreateRes(@SThreadExternalTerminate);
  FTerminated := True;
  TerminatedSet;
end;

function TRyuThread.WaitFor: LongWord;
{$IFDEF MSWINDOWS}
var
  H: array[0..1] of THandle;
  WaitResult: Cardinal;
  Msg: TMsg;
begin
  if FExternalThread then
    raise EThread.CreateRes(@SThreadExternalWait);
  H[0] := FHandle;
  if CurrentThread.ThreadID = MainThreadID then
  begin
    WaitResult := 0;
    H[1] := SyncEvent;
    repeat
      { This prevents a potential deadlock if the background thread
        does a SendMessage to the foreground thread }
      if WaitResult = WAIT_OBJECT_0 + 2 then
        PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE);
      WaitResult := MsgWaitForMultipleObjects(2, H, False, 1000, QS_SENDMESSAGE);
      CheckThreadError(WaitResult <> WAIT_FAILED);
      if WaitResult = WAIT_OBJECT_0 + 1 then
        CheckSynchronize;
    until WaitResult = WAIT_OBJECT_0;
  end else WaitForSingleObject(H[0], INFINITE);
  CheckThreadError(GetExitCodeThread(H[0], Result));
end;
{$ENDIF}
{$IFDEF POSIX}
var
  X: Pointer;
  ID: pthread_t;
begin
  if FExternalThread then
    raise EThread.CreateRes(@SThreadExternalWait);
  ID := pthread_t(FThreadID);
  if CurrentThread.ThreadID = MainThreadID then
    while not FFinished do
      CheckSynchronize(1000);
  FThreadID := 0;
  X := @Result;
  CheckThreadError(pthread_join(ID, X));
end;
{$ENDIF}

class procedure TRyuThread.NameThreadForDebugging(AThreadName: AnsiString;
  AThreadID: TThreadID);
{$IFDEF MSWINDOWS}
type
  TThreadNameInfo = record
    FType: LongWord;     // must be 0x1000
    FName: PAnsiChar;    // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;
var
  ThreadNameInfo: TThreadNameInfo;
begin
  if IsDebuggerPresent then
  begin
    ThreadNameInfo.FType := $1000;
    ThreadNameInfo.FName := PAnsiChar(AThreadName);
    ThreadNameInfo.FThreadID := AThreadID;
    ThreadNameInfo.FFlags := 0;

    try
      RaiseException($406D1388, 0, sizeof(ThreadNameInfo) div sizeof(LongWord), @ThreadNameInfo);
    except
    end;
  end;
end;
{$ENDIF}


{$IFDEF LINUX}
begin
end;
{$ENDIF}
{$IFDEF MACOS}
const
  cExceptionMessage = 'Type=$1000,Name=%s,ThreadID=%d,Flags=0';
begin
  if getenv('EMB_MACOSX_DBK_PRESENT') <> nil then
  begin
    try
      raise EThreadNameException.Create(Format(cExceptionMessage, [String(AThreadName), AThreadID]));
    except
    end;
  end;
end;
{$ENDIF}

initialization
  TRyuThread.StackSize := 0;
end.
