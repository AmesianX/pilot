program Sender;

uses
  ExceptionLog,
  Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  _fmLog in '..\..\Lib\Dialog\_fmLog.pas' {fmLog},
  DeskCamSampleUtils in '..\..\Lib\DeskCamSampleUtils.pas',
  DeskCamAddonUtils in '..\..\..\DeskCamAddonUtils.pas',
  DeskCamSender in '..\..\..\DeskCamSender.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
