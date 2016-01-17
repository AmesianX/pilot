program Server;

uses
  Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  VirtualSocketUtils in '..\..\Lib\VirtualSocketUtils.pas',
  VirtualSocketServer in '..\..\Lib\VirtualSocketServer.pas',
  VirtualSocketServerProviderIndy9 in '..\..\Lib\VirtualSocketServerProviderIndy9.pas',
  TextServer in 'TextServer.pas',
  DeskServer in 'DeskServer.pas',
  CamServer in 'CamServer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
