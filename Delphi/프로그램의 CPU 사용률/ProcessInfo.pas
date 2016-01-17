{***********************************
Description:
  Source code of ProcessInfo component.

Author  : Ali Keshavarz
Contact : vcldeveloper@gmail.com
Date    : 2009/02/05
History : 2009/10/17 - Enumerators are added for Threads, Modules, and Processes.
                     - TProcessItem.UserName is added.
                     - TProocessInfo.AdjustDebugPrivilage is added.
                     - TThreadItem.ToString & TProcessItem.ToString are added.
                     - Now supports Delphi 7,2007,2009, 2010.
          2010/07/26 - CPU usage is added to TProcessItem.
                       Is64Bits is added to TProcessItem.
                       IsAccessible is added to TProcessItem.
                       Setting thread priority is added.
                       Setting process base priority class is added.
          2010/09/01 - PROCESS_MEMORY_COUNTERS_EX is supported for WinXP SP2 and above.
          2010/09/27 - CloseProcess is added to TProcessItem to close a process in a normal way.
          2010/11/14 - TProcessInfo.FullPath now uses GetModuleFileNameEx API.
          2010/12/26 - ProcessInformation global function is added to make accessing to
                       processes information easier. It returns a global instance of
                       TProcessInfo.
                       TProcessInfo.CurrentProcess is added. It returns a TProcessItem
                       instance representing current running process.
          2011/09/07 - Win64 support is added (Delphi XE2).                       

License:
  This work is licensed under the Creative Commons Attribution 3.0 Unported License.
  To view a copy of this license, visit http://creativecommons.org/licenses/by/3.0/
  or send a letter to Creative Commons, 171 Second Street, Suite 300, San Francisco,
  California, 94105, USA.
***********************************}

unit ProcessInfo;

interface

{$INCLUDE ProcessInfo.inc}

uses
  SysUtils, Classes, Windows, Messages, PsAPI, ExtCtrls;

const
  Default_Update_Interval = 1000;

  {Defining contstats for priority class of processes}

  ABOVE_NORMAL_PRIORITY_CLASS = $00008000;
  BELOW_NORMAL_PRIORITY_CLASS = $00004000;
  HIGH_PRIORITY_CLASS = $00000080;
  IDLE_PRIORITY_CLASS = $00000040;
  NORMAL_PRIORITY_CLASS = $00000020;
  PROCESS_MODE_BACKGROUND_BEGIN = $00100000;
  PROCESS_MODE_BACKGROUND_END = $00200000;
  REALTIME_PRIORITY_CLASS = $00000100;

  {Defining contstats for thread priority}

  THREAD_MODE_BACKGROUND_BEGIN = $00010000;
  THREAD_MODE_BACKGROUND_END = $00020000;
  THREAD_PRIORITY_ABOVE_NORMAL = 1;
  THREAD_PRIORITY_BELOW_NORMAL = -1;
  THREAD_PRIORITY_HIGHEST = 2;
  THREAD_PRIORITY_IDLE = -15;
  THREAD_PRIORITY_LOWEST = -2;
  THREAD_PRIORITY_NORMAL = 0;
  THREAD_PRIORITY_TIME_CRITICAL = 15;

type
  TThreadItem = class(TCollectionItem)
  private
    FBasePriority: DWORD;
    FOwnerProcessID: DWORD;
    FThreadID: DWORD;
    procedure SetBasePriority(Value: DWORD);
  public
    function ResumeThread: Boolean;
    function SuspendThread: Boolean;
    function TerminateThread: Boolean;
    function ToString: string; {$IFDEF DELPHI2007UP}override;{$ENDIF}
    property BasePriority: DWord read FBasePriority write SetBasePriority;
    property OwnerProcessID: DWord read FOwnerProcessID;
    property ThreadID: DWord read FThreadID;
  end;

  {$IFDEF DELPHI2006UP}
  TThreadEnumerator = record
  private
    FIndex: Integer;
    FCollection: TCollection;
  public
    constructor Create(ACollection: TCollection);
    function GetCurrent: TThreadItem; inline;
    function MoveNext: Boolean; inline;
    property Current: TThreadItem read GetCurrent;
  end;
  {$ENDIF}

  TThreadCollection = class(TOwnedCollection)
  private
    FLastUpdate: TDateTime;
    function GetItems(Index: Integer): TThreadItem;
    procedure SetItems(Index: Integer; Value: TThreadItem);
  protected
    function Add: TThreadItem; reintroduce;
    procedure Clear; reintroduce;
    procedure Delete(Index: Integer); reintroduce;
    function Insert(Index: integer): TThreadItem; reintroduce;
  public
    constructor Create(AOwner: TPersistent);
    function FindByID(ID: DWord): TThreadItem;
    {$IFDEF DELPHI2006UP}
    function GetEnumerator: TThreadEnumerator;
    {$ENDIF}
    procedure UpdateThreads;
    property Items[Index: Integer]: TThreadItem read GetItems write SetItems;  default;
    property LastUpdate: TDateTime read FLastUpdate;
  end;

  TModuleItem = class(TCollectionItem)
  private
    FBaseAddress: DWord;
    FBaseSize: DWord;
    FHandle: THandle;
    FLoadCount: DWord;
    FModuleID: DWord;
    FModuleName: string;
    FModulePath: TFileName;
    FProcessID: DWord;
  public
    property BaseAddress: DWord read FBaseAddress;
    property BaseSize: DWord read FBaseSize;
    property Handle: THandle read FHandle;
    property LoadCount: DWord read FLoadCount;
    property ModuleID: DWord read FModuleID;
    property ModuleName: string read FModuleName;
    property ModulePath: TFileName read FModulePath;
    property ProcessID: DWord read FProcessID;
  end;

  {$IFDEF DELPHI2006UP}
  TModuleEnumerator = record
  private
    FIndex: Integer;
    FCollection: TCollection;
  public
    constructor Create(ACollection: TCollection);
    function GetCurrent: TModuleItem; inline;
    function MoveNext: Boolean; inline;
    property Current: TModuleItem read GetCurrent;
  end;
  {$ENDIF}

  TModuleCollection = class(TOwnedCollection)
  private
    FLastUpdate: TDateTime;
    function GetItems(Index: Integer): TModuleItem;
    procedure SetItems(Index: Integer; Value: TModuleItem);
  protected
    function Add: TModuleItem; reintroduce;
    procedure Clear; reintroduce;
    procedure Delete(Index: Integer); reintroduce;
    function Insert(Index: integer): TModuleItem; reintroduce;
  public
    constructor Create(AOwner: TPersistent);
    function FindByHandle(AHandle: THandle): TModuleItem;
    function FindByID(ID: DWord): TModuleItem;
    function FindByName(const AName: string): TModuleItem;
    {$IFDEF DELPHI2006UP}
    function GetEnumerator: TModuleEnumerator;
    {$ENDIF}
    procedure UpdateModules;
    property Items[Index: Integer]: TModuleItem read GetItems write SetItems; default;
    property LastUpdate: TDateTime read FLastUpdate;
  end;

   {$IF CompilerVersion < 23}
   SIZE_T = NativeUInt;
   {$IFEND}

  {Definition for PROCESS_MEMORY_COUNTERS_EX which is not included in PsAPI.pas}

  {$EXTERNALSYM _PROCESS_MEMORY_COUNTERS_EX}
  _PROCESS_MEMORY_COUNTERS_EX = packed record
    cb: DWORD;
    PageFaultCount: DWORD;
    PeakWorkingSetSize: SIZE_T;
    WorkingSetSize: SIZE_T;
    QuotaPeakPagedPoolUsage: SIZE_T;
    QuotaPagedPoolUsage: SIZE_T;
    QuotaPeakNonPagedPoolUsage: SIZE_T;
    QuotaNonPagedPoolUsage: SIZE_T;
    PagefileUsage: SIZE_T;
    PeakPagefileUsage: SIZE_T;
    PrivateUsage: SIZE_T;
  end;
  {$EXTERNALSYM PROCESS_MEMORY_COUNTERS_EX}
  PROCESS_MEMORY_COUNTERS_EX = _PROCESS_MEMORY_COUNTERS_EX;
  {$EXTERNALSYM PPROCESS_MEMORY_COUNTERS_EX}
  PPROCESS_MEMORY_COUNTERS_EX = ^_PROCESS_MEMORY_COUNTERS_EX;
  TProcessMemoryCounters_EX = _PROCESS_MEMORY_COUNTERS_EX;
  PProcessMemoryCounters_EX = ^_PROCESS_MEMORY_COUNTERS_EX;

  TMemoryInfo = record
    PageFaultCount: DWORD;
    PeakWorkingSetSize: DWORD;
    WorkingSetSize: DWORD;
    QuotaPeakPagedPoolUsage: DWORD;
    QuotaPagedPoolUsage: DWORD;
    QuotaPeakNonPagedPoolUsage: DWORD;
    QuotaNonPagedPoolUsage: DWORD;
    PagefileUsage: DWORD;
    PeakPagefileUsage: DWORD;
    PrivateUsage: DWORD;
    PrivateWorkingSetSize : DWORD;
  end;

  TProcessItem = class(TCollectionItem)
  private
    FExeFile: TFileName;
    FModules: TModuleCollection;
    FParentProcessID: DWORD;
    FPriorityClassBase: DWORD;
    FProcessID: DWord;
    FThreads: TThreadCollection;
    FThreadsCount: Integer;
    function GetCpuUsage: Cardinal;
    function GetIs64Bits: Boolean;
    function GetProcessTimes(Index: Integer): TDateTime;
    function GetFullPath: TFileName;
    function GetIsAccessible: Boolean;
    function GetMemoryInfo: TMemoryInfo;
    function GetModules: TModuleCollection;
    function GetThreads: TThreadCollection;
    function GetUserName: string;
    procedure SetPriorityClassBase(Value: DWORD);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function CloseProcess: Boolean;
    function TerminateProcess: Boolean;
    function ToString: string; {$IFDEF DELPHI2007UP}override;{$ENDIF}
    property CpuUsage: DWord read GetCpuUsage;
    property CreationTime: TDateTime index 0 read GetProcessTimes;
    property ExeFile: TFileName read FExeFile;
    property FullPath: TFileName read GetFullPath;
    property IsAccessible: Boolean read GetIsAccessible;
    property Is64Bits: Boolean read GetIs64Bits;
    property KernelTime: TDateTime index 1 read GetProcessTimes;
    property MemoryInfo: TMemoryInfo read GetMemoryInfo;
    property Modules: TModuleCollection read GetModules;
    property ParentProcessID: DWord read FParentProcessID;
    property PriorityClassBase: DWORD read FPriorityClassBase write SetPriorityClassBase;
    property ProcessID: DWord read FProcessID;
    property Threads: TThreadCollection read GetThreads;
    property ThreadsCount: Integer read FThreadsCount;
    property UserName: string read GetUserName;
    property UserTime: TDateTime index 2 read GetProcessTimes;
  end;

  {$IFDEF DELPHI2006UP}
  TProcessEnumerator = record
  private
    FIndex: Integer;
    FCollection: TCollection;
  public
    constructor Create(ACollection: TCollection);
    function GetCurrent: TProcessItem; inline;
    function MoveNext: Boolean; inline;
    property Current: TProcessItem read GetCurrent;
  end;
  {$ENDIF}

  TProcessCollection = class(TOwnedCollection)
  private
    FLastUpdate: TDateTime;
    function GetItems(Index: Integer): TProcessItem;
    procedure SetItems(Index: Integer; Value: TProcessItem);
  protected
    function Add: TProcessItem; reintroduce;
    procedure Clear; reintroduce;
    procedure Delete(Index: Integer); reintroduce;
    function Insert(Index: integer): TProcessItem; reintroduce;
  public
    constructor Create(AOwner: TPersistent);
    function FindByID(ID: DWord): TProcessItem;
    function FindByName(AName: string): TProcessItem;
    {$IFDEF DELPHI2006UP}
    function GetEnumerator: TProcessEnumerator;
    {$ENDIF}
    procedure UpdateProcesses;
    property Items[Index: Integer]: TProcessItem read GetItems write SetItems; default;
    property LastUpdate: TDateTime read FLastUpdate;
  end;

  TOnBeforeUpdateEvent = procedure (Sender: TObject; var Cancel: Boolean) of object;

  TProcessInfo = class(TComponent)
  private
    FCurrentProcess : TProcessItem;
    FOnAfterUpdateList: TNotifyEvent;
    FOnBeforeUpdateList: TOnBeforeUpdateEvent;
    FRunningProcesses: TProcessCollection;
    FTimer: TTimer;
    function GetActive: Boolean;
    function GetCurrProcess: TProcessItem;
    function GetRunningProcesses: TProcessCollection;
    function GetUpdateInterval: Cardinal;
    procedure SetActive(Value: Boolean);
    procedure SetUpdateInterval(Value: Cardinal);
  protected
    function AdjustDebugPrivilege: Boolean;
    procedure DoOnAfterUpdateList; dynamic;
    procedure DoOnBeforeUpdateList(var Cancel: Boolean); dynamic;
    procedure DoOnTimer(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateList;
    property CurrentProcess: TProcessItem read GetCurrProcess;
    property RunningProcesses: TProcessCollection read GetRunningProcesses;
  published
    property Active: Boolean read GetActive write SetActive default False;
    property UpdateInterval: Cardinal read GetUpdateInterval write SetUpdateInterval default Default_Update_Interval;
    property OnAfterUpdateList: TNotifyEvent read FOnAfterUpdateList write FOnAfterUpdateList;
    property OnBeforeUpdateList: TOnBeforeUpdateEvent read FOnBeforeUpdateList write FOnBeforeUpdateList;
  end;

function GetNumberOfProcessors: Cardinal;
function OpenThread(dwDesiredAccess: DWORD; bInheritHandle: BOOL; dwThreadId: DWORD): THandle; stdcall; external kernel32 name 'OpenThread';

function ProcessInformation: TProcessInfo;

var
  NumberOfProcessors : Cardinal = 0;

procedure Register;


implementation

uses TlHelp32;

{$WARN SYMBOL_PLATFORM OFF}
{$IFNDEF DELPHI2007UP}
type
  PTokenUser = ^_TOKEN_USER;
  _TOKEN_USER = record
    User : TSIDAndAttributes;
  end;
{$ENDIF}

var
  /// Global TProcessInfo instance to be used by ProcessInformation() global function.
  PrivateProcessInfo : TProcessInfo;

/// Registers ProcessInfo component.
procedure Register;
begin
  RegisterComponents('Ali', [TProcessInfo]);
end;

/// Returns a global TProcessInfo instance, in case user do not want to create
/// or maintain an object instance him/herself.
function ProcessInformation: TProcessInfo;
begin
  if not Assigned(PrivateProcessInfo) then
    PrivateProcessInfo := TProcessInfo.Create(nil);
  Result := PrivateProcessInfo;
end;

/// <summary>
/// Returns number of processors available.
/// </summary>
function GetNumberOfProcessors: Cardinal;
var
  SysInfo : SYSTEM_INFO;
begin
  GetSystemInfo(SysInfo);
  Result := SysInfo.dwNumberOfProcessors;
end;

{
****************************** TThreadItem *******************************
}
/// <summary>
/// Decreases suspend count of the thread. If suspend count is zero, then the
/// thread is resumed.
/// </summary>
function TThreadItem.ResumeThread: Boolean;
const
  THREAD_SUSPEND_RESUME = $0002;
var
  hThread : THandle;
  SuspendCount : Cardinal;
begin
  Result := False;
  hThread := OpenThread(THREAD_SUSPEND_RESUME,False,Self.FThreadID);
  if hThread > 0 then
  try
    //ResumeThread returns value of suspend count before this ResumeThread call.
    SuspendCount := Windows.ResumeThread(hThread);
    Result := (SuspendCount > 0);
  finally
    CloseHandle(hThread);
  end;
end;

procedure TThreadItem.SetBasePriority(Value: DWORD);
var
  hThread : THandle;
begin
  //Get handle of the thread by its ID.
  hThread := OpenThread(PROCESS_SET_INFORMATION,False,FThreadID);
  if hThread > 0 then
  try
    Win32Check(Windows.SetThreadPriority(hThread,Value));
    FBasePriority := Value;
  finally
    CloseHandle(hThread);
  end;
end;

/// <summary>
/// Suspends the thread and increases its suspend count.
/// </summary>
function TThreadItem.SuspendThread: Boolean;
const
  THREAD_SUSPEND_RESUME = $0002;
var
  hThread : THandle;
begin
  Result := False;
  hThread := OpenThread(THREAD_SUSPEND_RESUME,False,Self.FThreadID);
  if hThread > 0 then
  try
    Windows.SuspendThread(hThread);
    Result := True;
  finally
    CloseHandle(hThread);
  end
  else
   RaiseLastOSError;
end;

function TThreadItem.TerminateThread: Boolean;
const
  THREAD_TERMINATE = $0001;
var
  hThread : THandle;
begin
  Result := False;
  hThread := OpenThread(THREAD_TERMINATE,False,Self.FThreadID);
  if hThread > 0 then
  try
    Result := Windows.TerminateThread(hThread,0);
  finally
    CloseHandle(hThread);
  end;
end;

function TThreadItem.ToString: string;
begin
  Result := IntToStr(Self.ThreadID);
end;

{
****************************** TThreadEnumerator *******************************
}
{$IFDEF DELPHI2006UP}
constructor TThreadEnumerator.Create(ACollection: TCollection);
begin
  FCollection := ACollection;
  FIndex := -1;
end;

function TThreadEnumerator.GetCurrent: TThreadItem;
begin
  Result := FCollection.Items[FIndex] as TThreadItem;
end;

function TThreadEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FCollection.Count-1;
  if Result then
    Inc(FIndex);
end;
{$ENDIF}

{
****************************** TThreadCollection *******************************
}
constructor TThreadCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TThreadItem);
  UpdateThreads;
end;

function TThreadCollection.Add: TThreadItem;
begin
  Result := inherited Add as TThreadItem;
end;

procedure TThreadCollection.Clear;
begin
  inherited Clear;
end;

procedure TThreadCollection.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

function TThreadCollection.FindByID(ID: DWord): TThreadItem;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Self.Count - 1 do
    if Items[I].FThreadID = ID then
    begin
      Result := Items[I];
      Break;
    end;
end;

{$IFDEF DELPHI2006UP}
function TThreadCollection.GetEnumerator: TThreadEnumerator;
begin
  Result := TThreadEnumerator.Create(Self);
end;
{$ENDIF}

function TThreadCollection.GetItems(Index: Integer): TThreadItem;
begin
  Result := inherited Items[Index] as TThreadItem;
end;

function TThreadCollection.Insert(Index: integer): TThreadItem;
begin
  Result := inherited Insert(Index) as TThreadItem;
end;

procedure TThreadCollection.SetItems(Index: Integer; Value: TThreadItem);
begin
  TThreadItem(Items[Index]).Assign(Value);
end;

procedure TThreadCollection.UpdateThreads;
var
  hSnapShot: THandle;
  Te32: TThreadEntry32;
  ThreadItem: TThreadItem;
  ProcessID : Cardinal;
begin
  ProcessID := 0;
  //If a TProcessItem is specified as Owner, then return its threads; otherwise, return all running
  //threads in all processes.
  if Owner is TProcessItem then
    ProcessID := TProcessItem(Owner).ProcessID;

  hSnapShot := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD,ProcessID);
  if hSnapShot <> INVALID_HANDLE_VALUE then
  begin
    try
      BeginUpdate;
      try
        Clear;
        Te32.dwSize := SizeOf(Te32);
        if Thread32First(hSnapshot,Te32) then
        repeat
          if Te32.th32OwnerProcessID = ProcessID then
          begin
            ThreadItem := Add;
            ThreadItem.FBasePriority   := Te32.tpBasePri;
            ThreadItem.FOwnerProcessID := Te32.th32OwnerProcessID;
            ThreadItem.FThreadID       := Te32.th32ThreadID;
          end;

          Te32.dwSize := SizeOf(Te32);
        until Thread32Next(hSnapshot,Te32) = False;
      finally
        EndUpdate;
      end;
    finally
      CloseHandle(hSnapShot);
      FLastUpdate := Now;
    end;
  end
  else
    RaiseLastOSError;
end;

{
****************************** TModuleEnumerator *******************************
}
{$IFDEF DELPHI2006UP}
constructor TModuleEnumerator.Create(ACollection: TCollection);
begin
  FCollection := ACollection;
  FIndex := -1;
end;

function TModuleEnumerator.GetCurrent: TModuleItem;
begin
  Result := FCollection.Items[FIndex] as TModuleItem;
end;

function TModuleEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FCollection.Count-1;
  if Result then
    Inc(FIndex);
end;
{$ENDIF}

{
****************************** TModuleCollection *******************************
}
constructor TModuleCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TModuleItem);
  UpdateModules;
end;

function TModuleCollection.Add: TModuleItem;
begin
  Result := inherited Add as TModuleItem;
end;

procedure TModuleCollection.Clear;
begin
  inherited Clear;
end;

procedure TModuleCollection.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

/// <summary> Finds a module by its handle </summary>
function TModuleCollection.FindByHandle(AHandle: THandle): TModuleItem;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Count - 1 do
    if Items[I].FHandle = AHandle then
    begin
      Result := Items[I];
      Break;
    end;
end;

/// <summary> Finds a module by its ID </summary>
function TModuleCollection.FindByID(ID: DWord): TModuleItem;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Count - 1 do
    if Items[I].FModuleID = ID then
    begin
      Result := Items[I];
      Break;
    end;
end;

/// <summary> Finds a module by its name </summary>
function TModuleCollection.FindByName(const AName: string): TModuleItem;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Count - 1 do
    if SameText(Items[I].FModuleName,AName) then
    begin
      Result := Items[I];
      Break;
    end;
end;

{$IFDEF DELPHI2006UP}
function TModuleCollection.GetEnumerator: TModuleEnumerator;
begin
  Result := TModuleEnumerator.Create(Self);
end;
{$ENDIF}

function TModuleCollection.GetItems(Index: Integer): TModuleItem;
begin
  Result := inherited Items[Index] as TModuleItem;
end;

function TModuleCollection.Insert(Index: integer): TModuleItem;
begin
  Result := inherited Insert(Index) as TModuleItem;
end;

procedure TModuleCollection.SetItems(Index: Integer; Value: TModuleItem);
begin
  TModuleItem(Items[Index]).Assign(Value);
end;

procedure TModuleCollection.UpdateModules;
var
  hSnapShot: THandle;
  Me32: TModuleEntry32;
  ModuleItem: TModuleItem;
  ProcessID : Cardinal;
begin
  ProcessID := 0;
  //If a TProcessItem is specified as Owner, then return its modules; otherwise, return all loaded
  //modules in all processes.
  if Owner is TProcessItem then
    ProcessID := TProcessItem(Owner).ProcessID;

  hSnapShot := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE,ProcessID);
  if hSnapShot <> INVALID_HANDLE_VALUE then
  begin
    try
      BeginUpdate;
      try
        Clear;
        Me32.dwSize := SizeOf(Me32);
        if Module32First(hSnapshot,Me32) then
        repeat
          ModuleItem := Add;
          ModuleItem.FBaseAddress := NativeInt(Me32.modBaseAddr);
          ModuleItem.FBaseSize    := Me32.modBaseSize;
          ModuleItem.FHandle      := Me32.hModule;
          ModuleItem.FLoadCount   := Me32.GlblcntUsage;
          ModuleItem.FModuleID    := Me32.th32ModuleID;
          ModuleItem.FModuleName  := Trim(Me32.szModule);
          ModuleItem.FModulePath  := Trim(Me32.szExePath);
          ModuleItem.FProcessID   := Me32.th32ProcessID;

          Me32.dwSize := SizeOf(Me32);
        until Module32Next(hSnapshot,Me32) = False;
      finally
        EndUpdate;
      end;
    finally
      CloseHandle(hSnapShot);
      FLastUpdate := Now;
    end;
  end
  else
    RaiseLastOSError;
end;

{
********************************* TProcessItem *********************************
}
constructor TProcessItem.Create(Collection: TCollection);
begin
  inherited;
  FModules := nil;
  FThreads := nil;
end;

destructor TProcessItem.Destroy;
begin
  FreeAndNil(FModules);
  FreeAndNil(FThreads);

  inherited;
end;

/// <summary>
/// Tries to close the process in a normal way. CloseProcess waits about 1 second
/// for the process to be closed; otherwise, it returns False.
/// </summary>
/// <returns> True if the process is closed within 1 second time period. </returns>
/// <remarks>
/// CloseProcess does not force the process to close. If you need to force a process,
/// you should call TerminateProcess instead of CloseProcess.
/// </remarks>
function TProcessItem.CloseProcess: Boolean;
  function EnumWinCallback(wHandle: HWND; lparam: LPARAM): Bool; stdcall;
  var
    WindowProcessID : Cardinal;
    PCurrentProcessID : ^NativeUInt;
  begin
    GetWindowThreadProcessId(wHandle,WindowProcessID);
    ///Current processID is a cardinal value. To send it inside an integer parameter
    ///  we are sending it as a pointer.
    PCurrentProcessID := Pointer(lparam);
    /// If current process ID and process owning the current enumerated window
    ///  are equal, then close the window.
    if WindowProcessID = PCurrentProcessID^ then
      PostMessage(wHandle, WM_CLOSE, 0, 0);
    Result := True;
  end;
var
  hProcess : THandle;
begin
  Result := False;
  hProcess := OpenProcess(SYNCHRONIZE + PROCESS_TERMINATE, False, FProcessID);
  if hProcess > 0 then
  try
    /// Enumerate all top-level windows and close windows belonging to the current
    /// process.
    EnumWindows(@EnumWinCallback,LPARAM(@FProcessID));
    Result := (WaitForSingleObject(hProcess, Default_Update_Interval) = WAIT_OBJECT_0);
  finally
    CloseHandle(hProcess);
  end;
end;

function TProcessItem.GetProcessTimes(Index: Integer): TDateTime;
var
  hProcess : THandle;
  ACreationTime,
  AKernelTime,
  AUserTime : TDateTime;
  CTime,
  ETime,
  KTime,
  UTime   : FILETIME;
  SysTime,
  LocalTime : SYSTEMTIME;
begin
  ACreationTime := 0.0;
  AKernelTime := 0.0;
  AUserTime := 0.0;

  //Get handle of the process by its ID.
  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION,False,FProcessID);
  if hProcess > 0 then
  try
    //Retrieve process times.
    Win32Check(Windows.GetProcessTimes(hProcess, CTime,ETime, KTime, UTime));

    //Convert times to local times.
    //Creation time
    FileTimeToSystemTime(CTime, SysTime);
    SystemTimeToTzSpecificLocalTime(nil,SysTime,LocalTime);
    ACreationTime := SystemTimeToDateTime(LocalTime);
    //Kernel time
    FileTimeToSystemTime(KTime, SysTime);
    SystemTimeToTzSpecificLocalTime(nil,SysTime,LocalTime);
    AKernelTime := SystemTimeToDateTime(LocalTime);
    //User time
    FileTimeToSystemTime(UTime, SysTime);
    SystemTimeToTzSpecificLocalTime(nil,SysTime,LocalTime);
    AUserTime := SystemTimeToDateTime(LocalTime);
  finally
    CloseHandle(hProcess);
  end;

  //Return result based on the property that called method.
  case Index of
    0 : Result := ACreationTime;
    1 : Result := AKernelTime;
    2 : Result := AUserTime;
    else Result := 0.0;
  end;
end;

function TProcessItem.GetCpuUsage: Cardinal;
const
  dwWaitTime = 50;
var
  hProcess : THandle;
  CTime,
  ETime,
  KTime,
  UTime   : FILETIME;
  FirstTime,
  SecondTime : Int64;
begin
  Result := 0;
  //Get handle of the process by its ID.
  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION,False,FProcessID);
  if hProcess > 0 then
  try
    {To calculate CPU usage, we need to get process  times twice, and calculate
     time difference. }

    //Retrieve process times for first time.
    Win32Check(Windows.GetProcessTimes(hProcess, CTime,ETime,KTime,UTime));
    FirstTime := Int64(KTime.dwLowDateTime or (KTime.dwHighDateTime shr 32)) +
                 Int64(UTime.dwLowDateTime or (UTime.dwHighDateTime shr 32));
    //wait for a few miliseconds
    Sleep(dwWaitTime);
    //Retrieve process times for the second time.
    Win32Check(Windows.GetProcessTimes(hProcess, CTime,ETime,KTime,UTime));
    SecondTime := Int64(KTime.dwLowDateTime or (KTime.dwHighDateTime shr 32)) +
                 Int64(UTime.dwLowDateTime or (UTime.dwHighDateTime shr 32));
    //Calculate CPU usage
    Result := Round(((SecondTime - FirstTime) / dwWaitTime) / 100 / NumberOfProcessors);
  finally
    CloseHandle(hProcess);
  end;
end;

/// <summary> Returns EXE path of the process. </summary>
function TProcessItem.GetFullPath: TFileName;
var
  hProcess: THandle;
begin
  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,False,FProcessID);
  if hProcess <> 0 then
  begin
    try
      SetLength(Result,MAX_PATH);
      FillChar(Result[1],Length(Result) * SizeOf(Char), 0);
      if GetModuleFileNameEx(hProcess,0,PChar(Result),Length(Result)) > 0 then
        Result := Trim(Result)
      else
       RaiseLastOSError;
    finally
      CloseHandle(hProcess)
    end;
  end
  else
    RaiseLastOSError;

//  hSnapShot := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE,FProcessID);
//  if hSnapShot <> INVALID_HANDLE_VALUE then
//  try
//    Me32.dwSize := SizeOf(Me32);
//    //First module loaded in each process is EXE file of the process.
//    if Module32First(hSnapshot,Me32) then
//      Result := Trim(Me32.szExePath);
//  finally
//    CloseHandle(hSnapShot);
//  end
//  else
//    RaiseLastOSError;
end;

function TProcessItem.GetIs64Bits: Boolean;
type
  TIsWow64Process = function(hProcess: THandle; Wow64Process: PBOOL): BOOL; stdcall;
var
  IsWow64Process : TIsWow64Process;
  hProcess : THandle;
  Wow64Process : BOOL;
begin
  Result := False;
  @IsWow64Process := GetProcAddress(GetModuleHandle('Kernel32'),'IsWow64Process');
  if Assigned(IsWow64Process) then
  begin
    hProcess := OpenProcess(PROCESS_QUERY_INFORMATION, False, FProcessID);
    try
      {$IFNDEF 64BIT}
      if hProcess <= 0 then
      begin
        Result := True;
        Exit;
      end
      else
      {$ENDIF}
      if IsWow64Process(hProcess,@Wow64Process) then
        Result := not Wow64Process;
    finally
      CloseHandle(hProcess);
    end;
  end;
end;

function TProcessItem.GetIsAccessible: Boolean;
var
  ProcessHandle: THandle;
begin
  ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION, False, FProcessID);
  Result := (ProcessHandle > 0);
  if Result then
    CloseHandle(ProcessHandle);
end;

/// <summary>
///   Returns memory information for the process.
/// </summary>
///
/// <remarks> Only works in Windows 2000 and above. </remarks>
function TProcessItem.GetMemoryInfo: TMemoryInfo;
var
  hProcess: THandle;
  ProcessMemCountersEx : PROCESS_MEMORY_COUNTERS_EX;
begin
  ZeroMemory(@Result,SizeOf(Result));
  ZeroMemory(@ProcessMemCountersEx,SizeOf(ProcessMemCountersEx));

  //Get handle of the process by its ID.
  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION,False,FProcessID);
  if hProcess > 0 then
  try
    {Retrieve the process memory information.}
    // If Windows is XP SP2 or above, then use PROCESS_MEMORY_COUNTERS_EX struct;
    // otherwise, use PROCESS_MEMORY_COUNTERS struct which does not contain PrivateUsage.
    if CheckWin32Version(5,1) then
    begin
      ProcessMemCountersEx.cb := SizeOf(ProcessMemCountersEx);
      Win32Check(GetProcessMemoryInfo(hProcess,@ProcessMemCountersEx,ProcessMemCountersEx.cb));
    end
    else
    begin
      ProcessMemCountersEx.cb := SizeOf(ProcessMemCountersEx) - SizeOf(SIZE_T);
      Win32Check(GetProcessMemoryInfo(hProcess,@ProcessMemCountersEx, ProcessMemCountersEx.cb));
    end;

    with Result do
    begin
      PageFaultCount := ProcessMemCountersEx.PageFaultCount;
      PeakWorkingSetSize := ProcessMemCountersEx.PeakWorkingSetSize;
      WorkingSetSize := ProcessMemCountersEx.WorkingSetSize;
      QuotaPeakPagedPoolUsage := ProcessMemCountersEx.QuotaPeakPagedPoolUsage;
      QuotaPagedPoolUsage := ProcessMemCountersEx.QuotaPagedPoolUsage;
      QuotaPeakNonPagedPoolUsage := ProcessMemCountersEx.QuotaPeakNonPagedPoolUsage;
      QuotaNonPagedPoolUsage := ProcessMemCountersEx.QuotaNonPagedPoolUsage;
      PagefileUsage := ProcessMemCountersEx.PagefileUsage;
      PeakPagefileUsage := ProcessMemCountersEx.PeakPagefileUsage;
      PrivateUsage := ProcessMemCountersEx.PrivateUsage;
      { TODO -oAli -cFEATURE :
      Calculate private working set size for each project.
      Refer to community contents: http://msdn.microsoft.com/en-us/library/aa965225(VS.85).aspx
      Currently the problem is implementing PSAPI_WORKING_SET_INFORMATION struct in Delphi. }
      PrivateWorkingSetSize := 0;
    end;
  finally
    CloseHandle(hProcess);
  end;
end;

function TProcessItem.GetModules: TModuleCollection;
begin
  //Populating modules list for the process is delayed until the first time user tries to get modules.
  if not Assigned(FModules) then
    FModules := TModuleCollection.Create(Self);

  Result := FModules;
end;

function TProcessItem.GetThreads: TThreadCollection;
begin
  //Populating threads list for the process is delayed until the first time user tries to get threads.
  if not Assigned(FThreads) then
    FThreads := TThreadCollection.Create(Self);

  Result := FThreads;
end;

function TProcessItem.GetUserName: string;
var
  ProcessHandle : THandle;
  ProcessToken : THandle;
  Token : PTokenUser;
  TokenLength : Cardinal;
  UserSize, DomainSize : Cardinal;
  peUse : SID_NAME_USE;
  User, Domain : string;
begin
  ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION, False, FProcessID);
  if ProcessHandle = 0 then
    Exit;

  try
    if OpenProcessToken(ProcessHandle,TOKEN_QUERY,ProcessToken) then
    try
      GetTokenInformation(ProcessToken,TokenUser,nil,0,TokenLength);
      Token := AllocMem(TokenLength);
      try
        if GetTokenInformation(ProcessToken,TokenUser,Token,TokenLength,TokenLength) then
        begin
          UserSize := 0;
          DomainSize := 0;
          LookupAccountSid(nil,Token^.User.Sid,nil,UserSize,nil,DomainSize,peUse);
          SetLength(User,UserSize);
          SetLength(Domain,DomainSize);
          if LookupAccountSid(nil,Token^.User.Sid,PChar(User),UserSize,PChar(Domain),DomainSize,peUse) then
            Result := Trim(Domain) + '\' + Trim(User);
        end;
      finally
        FreeMem(Token);
      end;
    finally
      CloseHandle(ProcessToken);
    end;
  finally
    CloseHandle(ProcessHandle);
  end;
end;

procedure TProcessItem.SetPriorityClassBase(Value: DWORD);
var
  hProcess : THandle;
begin
  //Get handle of the process by its ID.
  hProcess := OpenProcess(PROCESS_SET_INFORMATION,False,FProcessID);
  if hProcess > 0 then
  try
    //Set process base priority class.
    Win32Check(Windows.SetPriorityClass(hProcess,Value));
    FPriorityClassBase := Value;
  finally
    CloseHandle(hProcess);
  end;
end;

/// <summary>
/// Forces the process to terminate. The process will not have any chance to do
/// cleanup jobs before being terminated.
/// </summary>
/// <returns> True if the process is closed. </returns>
/// <remarks>
/// To terminate a process in a normal way (without forcing it), you should call
/// CloseProcess instead of TerminateProcess.
/// </remarks>
function TProcessItem.TerminateProcess: Boolean;
var
  hProcess : THandle;
begin
  Result := False;
  hProcess := OpenProcess(PROCESS_TERMINATE,False,FProcessID);
  if hProcess > 0 then
  try
    Result := Win32Check(Windows.TerminateProcess(hProcess,0));
  finally
    CloseHandle(hProcess);
  end;
end;

function TProcessItem.ToString: string;
begin
  Result := Self.ExeFile;
end;

{
****************************** TProcessEnumerator *******************************
}
{$IFDEF DELPHI2006UP}
constructor TProcessEnumerator.Create(ACollection: TCollection);
begin
  FCollection := ACollection;
  FIndex := -1;
end;

function TProcessEnumerator.GetCurrent: TProcessItem;
begin
  Result := FCollection.Items[FIndex] as TProcessItem;
end;

function TProcessEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FCollection.Count-1;
  if Result then
    Inc(FIndex);
end;
{$ENDIF}

{
****************************** TProcessCollection ******************************
}
constructor TProcessCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TProcessItem);
end;

function TProcessCollection.Add: TProcessItem;
begin
  Result := inherited Add as TProcessItem;
end;

procedure TProcessCollection.Clear;
begin
  inherited Clear;
end;

procedure TProcessCollection.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

/// <summary> Finds a process by its ID. </summary>
function TProcessCollection.FindByID(ID: DWord): TProcessItem;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Count - 1 do
    if Items[I].FProcessID = ID then
    begin
      Result := Items[I];
      Break;
    end;
end;

/// <summary> Finds a process by its name. </summary>
function TProcessCollection.FindByName(AName: string): TProcessItem;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Count - 1 do
    if SameText(Items[I].FExeFile,AName) then
    begin
      Result := Items[I];
      Break;
    end;
end;

{$IFDEF DELPHI2006UP}
function TProcessCollection.GetEnumerator: TProcessEnumerator;
begin
  Result := TProcessEnumerator.Create(Self);
end;
{$ENDIF}

function TProcessCollection.GetItems(Index: Integer): TProcessItem;
begin
  Result := inherited Items[Index] as TProcessItem;
end;

function TProcessCollection.Insert(Index: integer): TProcessItem;
begin
  Result := inherited Insert(Index) as TProcessItem;
end;

procedure TProcessCollection.SetItems(Index: Integer; Value: TProcessItem);
begin
  TProcessItem(Items[Index]).Assign(Value);
end;

procedure TProcessCollection.UpdateProcesses;
var
  hSnapShot: THandle;
  pe32: TProcessEntry32;
  ProcessItem: TProcessItem;
begin
  hSnapShot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
  if hSnapShot <> INVALID_HANDLE_VALUE then
  begin
    try
      BeginUpdate;
      try
        Clear;
        pe32.dwSize := SizeOf(pe32);
        if Process32First(hSnapshot,pe32) then
        repeat
          ProcessItem := Add;
          ProcessItem.FExeFile           := Trim(pe32.szExeFile);
          ProcessItem.FParentProcessID   := pe32.th32ParentProcessID;
          ProcessItem.FPriorityClassBase := pe32.pcPriClassBase;
          ProcessItem.FProcessID         := pe32.th32ProcessID;
          ProcessItem.FThreadsCount      := pe32.cntThreads;

          pe32.dwSize := SizeOf(pe32);
        until Process32Next(hSnapshot,pe32) = False;
      finally
        EndUpdate;
      end;
    finally
      CloseHandle(hSnapShot);
      FLastUpdate := Now;
    end;
  end
  else
    RaiseLastOSError;
end;

{
********************************* TProcessInfo *********************************
}
constructor TProcessInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FRunningProcesses := TProcessCollection.Create(Self);
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := Default_Update_Interval;
  FTimer.OnTimer := DoOnTimer;

  AdjustDebugPrivilege;
end;

destructor TProcessInfo.Destroy;
begin
  FreeAndNil(FRunningProcesses);
  FreeAndNil(FTimer);
  inherited;
end;

/// <summary>
/// Adjusts SE_DEBUG_NAME for current process, so that it can receive more info from processes.
/// </summary>
function TProcessInfo.AdjustDebugPrivilege: Boolean;
var
  TPPrev: _TOKEN_PRIVILEGES;
  TP: _TOKEN_PRIVILEGES;
  ReturnLength: Cardinal;
  {$IF CompilerVersion < 23}
  ProcessToken : Cardinal;
  {$ELSE}
  ProcessToken: NativeUInt;
  {$IFEND}
begin
  Result := False;
  if OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, ProcessToken) then
  try
    TP.PrivilegeCount := 1;
    if LookupPrivilegeValue(nil,'SeDebugPrivilege',TP.Privileges[0].LUID) then
    begin
      TP.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
      ReturnLength := 0;
      Result := AdjustTokenPrivileges(ProcessToken, False, TP, SizeOf(TPPrev), TPPrev, ReturnLength);
    end;
  finally
    CloseHandle(ProcessToken);
  end;
end;

procedure TProcessInfo.DoOnAfterUpdateList;
begin
  if Assigned(FOnAfterUpdateList) then
    FOnAfterUpdateList(Self);
end;

procedure TProcessInfo.DoOnBeforeUpdateList(var Cancel: Boolean);
begin
  if Assigned(FOnBeforeUpdateList) then
    FOnBeforeUpdateList(Self,Cancel);
end;

procedure TProcessInfo.DoOnTimer(Sender: TObject);
begin
  if not (csDesigning in ComponentState) then
    UpdateList;
end;

function TProcessInfo.GetActive: Boolean;
begin
  Result := FTimer.Enabled;
end;

function TProcessInfo.GetCurrProcess: TProcessItem;
begin
  if not Assigned(FCurrentProcess) then
    FCurrentProcess := RunningProcesses.FindByID(GetCurrentProcessId);
  Result := FCurrentProcess;
end;

function TProcessInfo.GetRunningProcesses: TProcessCollection;
begin
  if FRunningProcesses.Count = 0 then
    UpdateList;
  Result := FRunningProcesses;
end;

function TProcessInfo.GetUpdateInterval: Cardinal;
begin
  Result := FTimer.Interval;
end;

procedure TProcessInfo.SetActive(Value: Boolean);
begin
  FTimer.Enabled := Value;
  if Value then
    UpdateList;
end;

procedure TProcessInfo.SetUpdateInterval(Value: Cardinal);
begin
  if Value <> FTimer.Interval then
    FTimer.Interval := Value;
end;

procedure TProcessInfo.UpdateList;
var
  Cancel: Boolean;
begin
  Cancel := False;
  DoOnBeforeUpdateList(Cancel);
  //If user canceled update in OnBeforeUpdateList event-handler, then do not update list.
  if Cancel then Exit;
  try
    FRunningProcesses.UpdateProcesses;
    DoOnAfterUpdateList;
  except
    raise;
  end;
end;

initialization
  NumberOfProcessors := GetNumberOfProcessors;

finalization
  if Assigned(PrivateProcessInfo) then
    FreeAndNil(PrivateProcessInfo);

end.
