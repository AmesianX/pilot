unit _fmMain;

interface

uses
  uCpuUsage, ProcessInfo,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    Timer: TTimer;
    Button1: TButton;
    ListBox1: TListBox;
    procedure TimerTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FProcessInfo : TProcessInfo;
    procedure ProcessList;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

uses
  TlHelp32, PsAPI;

function GetCurrentProcessCpuUsage:Cardinal;
var
  HaveNext : BOOL;
  SHandle : THandle;
  CurrentProcessId : DWORD;
  Process32 : TProcessEntry32;
begin
  Result := 0;

  Process32.dwSize := SizeOf(TProcessEntry32);
  SHandle := CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS, 0);

  HaveNext := Process32First(SHandle, Process32);
  while HaveNext do begin
    if Pos('GOM.', Process32.szExeFile) > 0 then begin
//    if Process32.th32ProcessID = CurrentProcessId then begin
      Result := Process32.cntUsage;
      Break;
    end;

    HaveNext := Process32Next(SHandle, Process32);
  end;

  CloseHandle(SHandle);
end;

procedure TfmMain.Button1Click(Sender: TObject);
begin
  repeat
    Application.ProcessMessages;
  until false;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FProcessInfo := TProcessInfo.Create(Self);
end;

procedure TfmMain.ProcessList;
var
  hSnap : Thandle;
  ProcEntry : TProcessEntry32;
  EntryCount: integer;
begin
  hSnap := CreateToolhelp32Snapshot(TH32CS_SNAPALL, 0);
  ProcEntry.dwSize := SizeOf(TProcessEntry32);
  EntryCount := 0;
  if Process32First(hSnap, ProcEntry) then
  begin
  with ProcEntry do begin
    Inc(EntryCount);
    if EntryCount >= ListBox1.Items.Count then ListBox1.Items.Add('');
    ListBox1.Items[EntryCount-1] := StrPas(szEXEFile) + ' ' + IntToStr(cntUsage);
  end;
  while Process32Next(hSnap, ProcEntry) do
  with ProcEntry do
  begin
     inc(EntryCount);
    if EntryCount >= ListBox1.Items.Count then ListBox1.Items.Add('');
     ListBox1.Items[EntryCount-1] := StrPas(szEXEFile) + ' ' + IntToStr(cntUsage);
  end;
 end;
CloseHandle(hSnap);
end;

procedure TfmMain.TimerTimer(Sender: TObject);
begin
  ProcessList;
  Caption := IntToStr(FProcessInfo.CurrentProcess.CpuUsage);
end;

end.
