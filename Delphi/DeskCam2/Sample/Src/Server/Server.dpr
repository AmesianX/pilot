program Server;

uses
  ExceptionLog,
  Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  DeskCamSampleUtils in '..\..\Lib\DeskCamSampleUtils.pas',
  DeskCamServer in '..\..\..\DeskCamServer.pas',
  DeskCamAddonUtils in '..\..\..\DeskCamAddonUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
