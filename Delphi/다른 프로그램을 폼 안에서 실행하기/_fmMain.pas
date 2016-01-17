unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    mHWnd: HWND; // hWnd of Slave App
  public
  end;

var
  Form2: TForm2;

implementation

uses
  Disk;

{$R *.dfm}

function InstanceToWnd(Const TgtPID: DWORD): HWND;
var
  ThisHWnd: HWND;
  ThisPID: DWORD;
begin
  Result := 0;
  ThisPID := 0;

  ThisHWnd := FindWindow(nil, nil);

  while ThisHWnd <> 0 Do begin
    if GetParent(ThisHWnd) = 0 then begin
      GetWindowThreadProcessId(ThisHWnd, Addr(ThisPID));
      if ThisPID = TgtPID then begin
        Result := ThisHWnd;
        Break;
      end;
    end;

    ThisHWnd := GetWindow(ThisHWnd, GW_HWNDNEXT);
  end;
end;

function ExecCmd(Const cmdline: AnsiString): HWND;
var
  PI: PROCESS_INFORMATION;
  SI: STARTUPINFOA;
  Ret: LONGBOOL;
begin;
  Result := 0;

  ZeroMemory(Addr(PI), SizeOf(PI));

  ZeroMemory(Addr(SI), SizeOf(SI));
  SI.cb := SizeOf(SI);
  SI.wShowWindow := SW_MAXIMIZE;

  Ret := CreateProcessA(nil, PAnsiChar(cmdline), nil, nil, True, NORMAL_PRIORITY_CLASS, nil, nil, SI, PI);

  WaitForSingleObject(PI.hProcess, 500);

  if Ret then begin
    Result := InstanceToWnd(PI.dwProcessID);
    CloseHandle(PI.hThread);
    CloseHandle(PI.hProcess);
  end;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  LockWindowUpdate(GetDesktopWindow);

  mHWnd := ExecCmd('notepad.exe');

  if mHWnd = 0 then ShowMessage('Error starting the app');

  if mHWnd <> 0 then begin
    Winapi.Windows.SetParent(mHWnd, Handle);
    Winapi.Windows.MoveWindow(mHWnd, 0, 0, ClientWidth, ClientHeight, True);
    Winapi.Windows.SetFocus(mHWnd);
  end;

  LockWindowUpdate(0);
end;

end.
