program CreateP;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Windows,
  System.SysUtils;

const
  LOGON_WITH_PROFILE = $00000001;

function CreateProcessWithLogonW(
  lpUsername,
  lpDomain,
  lpPassword:PWideChar;
  dwLogonFlags:dword;
  lpApplicationName: PWideChar;
  lpCommandLine: PWideChar;
  dwCreationFlags: DWORD;
  lpEnvironment: Pointer;
  lpCurrentDirectory: PWideChar;
  lpStartupInfo: PStartupInfoW;
  lpProcessInformation: PProcessInformation
): BOOL; stdcall; external 'advapi32.dll';

function CreateProcessAsLogon(const User, PW, Application, CmdLine: WideString):
  LongWord;
var
  si           : TStartupInfoW;
  pif          : TProcessInformation;
begin
  ZeroMemory(@si, sizeof(si));
  si.cb := sizeof(si);
  si.dwFlags := STARTF_USESHOWWINDOW;
  si.wShowWindow := 1;

  SetLastError(0);
  CreateProcessWithLogonW(PWideChar(User), nil, PWideChar(PW),
    LOGON_WITH_PROFILE, nil, PWideChar(Application+' "'+CmdLine+'"'),
    CREATE_DEFAULT_ERROR_MODE, nil, nil, @si, @pif);
  Result := GetLastError;
end;


begin
  CreateProcessAsLogon('Test', '1234', 'cmd.exe', '/c dir');
end.
