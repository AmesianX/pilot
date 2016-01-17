program Pas2Html;

{$APPTYPE CONSOLE}

uses
  SysUtils, Disk,
  _dmMain in '_dmMain.pas' {dmMain: TDataModule};

begin
  // 현재를 포함한 하위 폴더 검색
  dmMain.Start(GetExecPath);
end.
