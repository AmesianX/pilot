program PushServerTest;

uses
  ExceptionLog,
  Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  Utils in '..\..\Lib\Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
