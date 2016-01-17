unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm4 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    m_hDesktop:HDESK;
    procedure Open;
    procedure Close;
    procedure Switch;
  public
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

function _CloseWinDowsProc(hWnd: hWnd; lParam: lParam): BOOL; stdcall;
var
  dwProcessId: DWORD;
begin
  GetWindowThreadProcessId(hWnd, dwProcessId);
  if GetCurrentProcessId = dwProcessId then
    PostMessage(hWnd, WM_CLOSE, 0, 0)
  else
    PostMessage(hWnd, WM_QUIT, 0, 0);
  Result := True;
end;

procedure TForm4.Button1Click(Sender: TObject);
var
  pi: TProcessInformation;
  si: TStartupInfo;
begin
  ZeroMemory(@pi, Sizeof(pi));

  ZeroMemory(@si, Sizeof(si));
  si.cb := Sizeof(TStartupInfo);
//  si.lpDesktop := 'HiMyTV';
//  si.dwFlags:= STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  si.wShowWindow:= SW_SHOW;

  CreateProcess(nil, 'explorer.exe', nil, nil, True, DETACHED_PROCESS or NORMAL_PRIORITY_CLASS, nil, nil, si, pi);
  Winapi.Windows.SetParent(pi.hProcess, Handle);
  SetWindowPos(pi.hProcess, HWND_TOP, 0, 0, Width, Height, SWP_NOSIZE);
end;

procedure TForm4.Close;
begin
  if m_hDesktop <> NULL then
    EnumDesktopWindows(m_hDesktop, @_CloseWinDowsProc, 0);
  CloseDesktop(m_hDesktop);
  m_hDesktop := 0;
end;

function CreateProcessX(sExecutable: string; sDirectory: string): string;
var
  pi: TProcessInformation;
  si: TStartupInfo;
begin
  FillMemory(@si, SizeOf(si), 0);
  si.cb := SizeOf(si);
  si.lpReserved := nil;
  si.lpDesktop := nil;
  si.lpTitle := nil;
  si.dwFlags := STARTF_USESHOWWINDOW;
  si.wShowWindow := SW_SHOW;
  si.cbReserved2 := 0;
  si.lpReserved2 := nil;

  CreateProcess(nil, PChar(sExecutable), nil, nil,
    False, CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil, si, pi);

  CloseHandle(pi.hProcess);
  CloseHandle(pi.hThread);
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
//  Open;
//  Switch;
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
//  Close;
end;

procedure TForm4.Open;
var
  dwRights: DWORD;
begin
  dwRights :=
    DESKTOP_READOBJECTS or
    DESKTOP_CREATEWINDOW or
    DESKTOP_CREATEMENU or
    DESKTOP_HOOKCONTROL or
    DESKTOP_JOURNALRECORD or
    DESKTOP_JOURNALPLAYBACK or
    DESKTOP_ENUMERATE or
    DESKTOP_WRITEOBJECTS or
    DESKTOP_SWITCHDESKTOP or
    STANDARD_RIGHTS_REQUIRED or
    READ_CONTROL or
    WRITE_DAC or
    WRITE_OWNER;

  m_hDesktop := CreateDesktop('HiMyTV', nil, nil, DF_ALLOWOTHERACCOUNTHOOK, dwRights, nil);
end;

procedure TForm4.Switch;
var
  pi: TProcessInformation;
  si: TStartupInfo;
begin
  SwitchDesktop(m_hDesktop);

  ZeroMemory(@pi, Sizeof(pi));

  ZeroMemory(@si, Sizeof(si));
  si.cb := Sizeof(TStartupInfo);
  si.lpDesktop := 'HiMyTV';
  si.dwFlags:= STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  si.wShowWindow:= SW_SHOW;

  CreateProcess(nil, 'explorer.exe', nil, nil, True, DETACHED_PROCESS or NORMAL_PRIORITY_CLASS, nil, nil, si, pi);
//  CloseHandle(pi.hProcess);
end;

end.
