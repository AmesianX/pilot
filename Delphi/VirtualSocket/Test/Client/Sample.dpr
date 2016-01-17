program Sample;

uses
  Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  VirtualSocketUtils in '..\..\Lib\VirtualSocketUtils.pas',
  VirtualSocketClient in '..\..\Lib\VirtualSocketClient.pas',
  VirtualSocketClientProviderIndy9 in '..\..\Lib\VirtualSocketClientProviderIndy9.pas',
  VirtualSocketServer in '..\..\Lib\VirtualSocketServer.pas',
  VirtualSocketServerProviderIndy9 in '..\..\Lib\VirtualSocketServerProviderIndy9.pas',
  DeskClient in 'DeskClient.pas',
  TextClient in 'TextClient.pas',
  CamClient in 'CamClient.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
