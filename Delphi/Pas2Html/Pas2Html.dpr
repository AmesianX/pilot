program Pas2Html;

{$APPTYPE CONSOLE}

uses
  SysUtils, Disk,
  _dmMain in '_dmMain.pas' {dmMain: TDataModule};

begin
  // ���縦 ������ ���� ���� �˻�
  dmMain.Start(GetExecPath);
end.
